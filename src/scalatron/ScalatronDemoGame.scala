package scalatron

import scalatron.core.Simulation.Time
import akka.actor.ActorSystem
import scala.util.Random
import scalatron.core._
import akka.dispatch.{Await, Future, ExecutionContext}
import akka.util.Duration
import java.awt.{Color, Frame}
import Game.arenaSize

/** This is a demo implementation of a pluggable game for Scalatron.
  *
  * To activate it, build it and place the resulting ScalatronDemoGame.jar file into Scalatron/bin/
  * Then launch Scalatron with
  *     java -jar Scalatron.jar -game ScalatronDemoGame
  *
  * This game implementation expects bot plug-ins that return a string representing a move command of the
  * format "x:y", such as "2:3". Here is an example bot implementation that you can test by pasting it into the
  * browser-based editor and clicking "Publish into Tournament":
  *
  *     // Demo bot for ScalatronDemoGame
  *     class ControlFunctionFactory { def create = (input: String) => "1:1" }
  *
  * Note that you may also want to delete the Reference bot from Scalatron/bots/Reference, since it implements
  * the BotWar protocol and not the one expected by this game.
  */


/** This is the class that will be extracted from the game plug-in by Scalatron in order to obtain a factory function. */
class GameFactory {
    def create() = Game
}



/** The Game trait is the main entry point of a game plug-in. It is made available to the Scalatron server via the
  * GameFactory class. Scalatron then invokes either runVisually() or runHeadless() to start the tournament loop.
  * Since no state is held, this can be a singleton object.
  */
case object Game extends scalatron.core.Game {
    val arenaSize = (512,512)

    def gameSpecificPackagePath = "scalatron.demoGame.botPlugin"

    def cmdArgList =
        Iterable.empty ++     // no command line args yet, otherwise: Iterable("key" -> "value", "key" -> "value")
            PermanentConfig.cmdArgList

    def runVisually(rounds: Int, scalatron: ScalatronInward) {
        // open an AWT frame so that we display some simple graphics
        val frame = new Frame("Scalatron Demo Game")
        frame.setSize(arenaSize._1,arenaSize._2)
        frame.setVisible( true )
        val graphics = frame.getGraphics

        // figure out configuration settings valid for all game rounds (we're primarily interested in "-steps")
        val permanentConfig = PermanentConfig.fromArgMap(scalatron.secureMode, scalatron.argMap)
        val stepsPerRound = permanentConfig.stepsPerRound

        // run the tournament loop: a sequence of game rounds
        val rnd = new Random()
        for(round <- 0 until rounds) {
            // load plug-ins
            val entityControllers = scalatron.freshEntityControllers

            // create initial game state, populated with one entity (bot) per entity controller
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
            for(step <- 0 until stepsPerRound) {
                // tell Scalatron about the most recent game state
                scalatron.postStepCallback(gameState)

                // render some graphics: black background with white squares representing bots
                graphics.setColor(Color.black)
                graphics.fillRect(0, 0, arenaSize._1,arenaSize._2)
                graphics.setColor(Color.white)
                graphics.drawString("time=" + gameState.time, 10, 40)
                gameState.entityMap.values.foreach(e => graphics.fillRect(e.position._1, e.position._2, 5, 5))

                // simulate next step
                gameState = gameState.step(scalatron.actorSystem, scalatron.executionContextForUntrustedCode).left.get

                Thread.sleep(25)    // artificial pause to slow down simulation
            }

            // tell Scalatron how this game round ended
            val roundResult = TournamentRoundResult(gameState.entityMap.values.map(e => (e.name, e.points)).toMap)
            scalatron.postRoundCallback(gameState, roundResult)
        }
    }

    def runHeadless(rounds: Int, scalatron: ScalatronInward) {
        // this will be the same as runVisual(), except no graphics :-)
        throw new UnsupportedOperationException
    }

    def startHeadless(entityControllers: Iterable[EntityController], roundConfig: RoundConfig, executionContextForUntrustedCode: ExecutionContext) = {
        // this will be invoked when a user starts a private, sandboxed game in the browser - currently unsupported
        throw new UnsupportedOperationException
    }
}

/** A GameEntity holds the state of an entity in the game. Since here we only have "master" entities, each of
  * which is associated with an entity controller, this is really primarily a wrapper for the entity controller.
  * We have a unique id and points, but they are not used yet. Each entity also has a position, which determines
  * where in the arena it resides and where it will show up on the screen.
  */
case class GameEntity(id: Int, entityController: EntityController, points: Int, position: (Int,Int)) {
    def name = entityController.name    // = plug-in name = player name
}


/** Currently there is only one command, for moving a bot around. */
sealed trait Command
case class MoveCommand(direction: (Int,Int)) extends Command


/** The GameState holds the state of the game, including the time, the state of the arena and the state of all
  * entities. It is immutable (at least as long as the control functions wrapped by the entity controllers are
  * immutable). Calling step() returns an updated copy representing the successor game state.
  */
case class GameState(time: Time, entityMap: Map[Int,GameEntity]) extends Simulation.State[GameState] {
    // a few utility methods for grabbing game entities
    def entities : Iterable[GameEntity] = entityMap.values
    def entity(id: Int) : Option[GameEntity] = entityMap.get(id)
    def entity(name: String) : Option[GameEntity] = entities.find(_.name == name)

    /** Returns the successor simulation state, generally an updated copy of this state.
      * We can use the actor system and/or the execution context to perform parallel processing. */
    def step(actorSystem: ActorSystem, executionContextForUntrustedCode: ExecutionContext) = {
        // compute the bot responses, concurrently and using the untrusted execution context
        // using the untrusted execution context means that bot plug-ins cannot (easily) access disk or network
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

        // process the bot responses: move each entity that requested it in the desired direction, as long as it
        // remains within the arena bounds.
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

    /** This method is invoked when the browser-based debugger wants to know about the entities associated with a
      * particular player (for display in the browser's "sandbox" debugging UI). Not really supported yet.
      */
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