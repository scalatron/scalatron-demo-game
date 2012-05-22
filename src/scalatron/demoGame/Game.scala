package scalatron.demoGame

import akka.dispatch.ExecutionContext
import java.awt.Frame
import scalatron.core.Simulation.Time
import akka.actor.ActorSystem
import scala.util.Random
import scalatron.core._

/** Implementation of the core.Game trait for the Scalatron BotWar game.
  * Since no state is held, this can be a singleton object.
  * It is made available to the Scalatron server via the GameFactory class.
  */
case object Game extends scalatron.core.Game
{
    def gameSpecificPackagePath = "scalatron.demoGame.botPlugin"

    def cmdArgList =
        Iterable.empty ++     // no command line args, otherwise: Iterable("key" -> "value", "key" -> "value")
            PermanentConfig.cmdArgList

    def runVisually(rounds: Int, scalatron: ScalatronInward) {
        val frame = new Frame("Scalatron Demo Game")
        val frameSize = (512,512)
        frame.setSize(frameSize._1,frameSize._2)
        val graphics = frame.getGraphics

        val permanentConfig = PermanentConfig.fromArgMap(scalatron.secureMode, scalatron.argMap)

        val rnd = new Random()
        for(round <- 0 until rounds) {
            // load plug-ins
            val entityControllers = scalatron.freshEntityControllers

            // create initial game state
            val initialEntities = entityControllers.map(e => GameEntity(e, 0, (rnd.nextInt(frameSize._1), rnd.nextInt(frameSize._2))))
            var gameState = GameState(0L, initialEntities)

            for(step <- 0 until permanentConfig.stepsPerRound) {
                // tell Scalatron about the most recent game state
                scalatron.postStepCallback(gameState)

                // render
                graphics.clearRect(0, 0, frameSize._1,frameSize._2)
                gameState.entities.foreach(e => graphics.fillRect(e.position._1, e.position._2, 5, 5))

                // simulate next step
                gameState = gameState.step(scalatron.actorSystem, scalatron.executionContextForUntrustedCode).left.get
            }

            // tell Scalatron how it ended
            val result = TournamentRoundResult(gameState.entities.map(e => (e.entityController.name, e.points)).toMap)
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

case class GameEntity(entityController: EntityController, points: Int, position: (Int,Int))

case class GameState(time: Time, entities: Iterable[GameEntity]) extends Simulation.UntypedState {
    def step(actorSystem: ActorSystem, executionContextForUntrustedCode: ExecutionContext) = Left(this)
    def entitiesOfPlayer(name: String) =
        entities
        .filter(_.entityController.name == name)
        .map(e => new Simulation.Entity {
            def id = 0
            def name = e.entityController.name
            def isMaster = true
            def mostRecentControlFunctionInput = ""
            def mostRecentControlFunctionOutput = Iterable.empty
            def debugOutput = ""
        })
}