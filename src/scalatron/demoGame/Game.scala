package scalatron.demoGame

import scalatron.core.Simulation.Time
import akka.actor.ActorSystem
import scala.util.Random
import scalatron.core._
import akka.dispatch.{Await, Future, ExecutionContext}
import akka.util.Duration
import java.awt.{Color, Frame}
import Game.arenaSize

/** Implementation of the core.Game trait for the Scalatron BotWar game.
  * Since no state is held, this can be a singleton object.
  * It is made available to the Scalatron server via the GameFactory class.
  */
case object Game extends scalatron.core.Game
{
    val arenaSize = (512,512)

    def gameSpecificPackagePath = "scalatron.demoGame.botPlugin"

    def cmdArgList =
        Iterable.empty ++     // no command line args, otherwise: Iterable("key" -> "value", "key" -> "value")
            PermanentConfig.cmdArgList

    def runVisually(rounds: Int, scalatron: ScalatronInward) {
        val frame = new Frame("Scalatron Demo Game")
        frame.setSize(arenaSize._1,arenaSize._2)
        frame.setVisible( true )
        val graphics = frame.getGraphics

        val permanentConfig = PermanentConfig.fromArgMap(scalatron.secureMode, scalatron.argMap)

        val rnd = new Random()
        for(round <- 0 until rounds) {
            // load plug-ins
            val entityControllers = scalatron.freshEntityControllers

            // create initial game state
            val initialEntityMap =
                entityControllers
                .zipWithIndex   // generate unique IDs
                .map(e => {
                    val id = e._2
                    val entity = GameEntity(id, e._1, 0, (rnd.nextInt(arenaSize._1), rnd.nextInt(arenaSize._2)))
                    (id, entity)
                })
                .toMap
            var gameState = GameState(0L, initialEntityMap)

            // run one game round consisting of several steps
            for(step <- 0 until permanentConfig.stepsPerRound) {
                // tell Scalatron about the most recent game state
                scalatron.postStepCallback(gameState)

                // render
                graphics.setColor(Color.black)
                graphics.fillRect(0, 0, arenaSize._1,arenaSize._2)
                graphics.setColor(Color.white)
                graphics.drawString("time=" + gameState.time, 10, 40)
                gameState.entityMap.values.foreach(e => graphics.fillRect(e.position._1, e.position._2, 5, 5))

                // simulate next step
                gameState = gameState.step(scalatron.actorSystem, scalatron.executionContextForUntrustedCode).left.get

                Thread.sleep(25)
            }

            // tell Scalatron how it ended
            val result = TournamentRoundResult(gameState.entityMap.values.map(e => (e.name, e.points)).toMap)
            scalatron.postRoundCallback(gameState, result)
        }
    }

    def runHeadless(rounds: Int, scalatron: ScalatronInward) {
        throw new UnsupportedOperationException
    }

    def startHeadless(entityControllers: Iterable[EntityController], roundConfig: RoundConfig, executionContextForUntrustedCode: ExecutionContext) = {
        throw new UnsupportedOperationException
    }
}

case class GameEntity(id: Int, entityController: EntityController, points: Int, position: (Int,Int)) {
    def name = entityController.name    // = plug-in name = player name
}


sealed trait Command
case class MoveCommand(direction: (Int,Int)) extends Command


case class GameState(time: Time, entityMap: Map[Int,GameEntity]) extends Simulation.State[GameState] {
    def entities : Iterable[GameEntity] = entityMap.values

    def entity(id: Int) : Option[GameEntity] = entityMap.get(id)
    def entity(name: String) : Option[GameEntity] = entities.find(_.name == name)

    def step(actorSystem: ActorSystem, executionContextForUntrustedCode: ExecutionContext) = {
        // compute the bot responses, concurrently and using the untrusted execution context
        val future = {
            implicit val executionContext = executionContextForUntrustedCode
            Future.traverse(entities)(entity => Future {
                try {
                    val response = entity.entityController.respond("input")     // we expect something like "2:3" representing a move request
                    val directionArray = response.split(':').map(_.toInt)       // "2:3" => Array(2,3)
                    Some((entity, MoveCommand((directionArray(0), directionArray(1)))))
                } catch {
                    case t: Throwable =>
                        System.err.println("error processing entity '%s': %s" format(entity.name, t.toString))
                        None
                }
            } )
        }
        val entityResponsePairs : Iterable[(GameEntity,Command)] = Await.result(future, Duration.Inf).flatten

        // process the bot responses
        var updatedState = copy(time = time + 1)
        entityResponsePairs.foreach(p => p._2 match {
            case MoveCommand(direction) =>
                val entityOpt = updatedState.entity(p._1.id)
                val updatedEntityOpt = entityOpt.map(e => {
                    val updatedPosition = (e.position._1 + direction._1, e.position._2 + direction._2)
                    val constrainedPosition =
                        if( updatedPosition._1 >= 0 && updatedPosition._1 < arenaSize._1 &&
                            updatedPosition._2 >= 0 && updatedPosition._2 < arenaSize._2 ) updatedPosition else e.position
                    e.copy(position = constrainedPosition)
                })
                updatedEntityOpt.foreach(e => updatedState = updatedState.copy(entityMap = updatedState.entityMap.updated(e.id, e)))

        })

        Left(updatedState)
    }

    def entitiesOfPlayer(name: String) =
        entity(name)
        .map(e => new Simulation.Entity {
            def id = 0
            def name = e.name
            def isMaster = true
            def mostRecentControlFunctionInput = ""
            def mostRecentControlFunctionOutput = Iterable.empty
            def debugOutput = ""
        })
        .toIterable
}