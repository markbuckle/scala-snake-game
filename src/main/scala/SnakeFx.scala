// all the imports we're going to need in the entire app
// (auto-import helps a lot, but adding them here in case of confusion)
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle

import scala.concurrent.Future
import scala.util.Random

object SnakeFx extends JFXApp3 {
  // Initial snake position as a List of (x,y) coordinates
  // The snake starts with 3 segments, facing right
  val initialSnake: List[(Double, Double)] = List(
    (250, 200), // head
    (225, 200), // body
    (200, 200) // tail
  )

  // Import execution context for Future operations
  import scala.concurrent.ExecutionContext.Implicits.global

  // Game loop function that updates the game state 12.5 times per second (1000/25*2 ms)
  def gameLoop(update: () => Unit): Unit =
    Future {
      update()
      Thread.sleep(1000 / 25 * 2) // Sleep for ~80ms
    }.flatMap(_ => Future(gameLoop(update))) // Recursively call gameLoop

  // State class representing the game state at any moment
  case class State(snake: List[(Double, Double)], food: (Double, Double)) {
    def newState(dir: Int): State = {
      val (x, y) = snake.head
      // Determine new head position based on direction
      // 1=up, 2=down, 3=left, 4=right
      val (newx, newy) = dir match {
        case 1 => (x, y - 25)   // Move up
        case 2 => (x, y + 25)   // Move down
        case 3 => (x - 25, y)   // Move left
        case 4 => (x + 25, y)   // Move right
        case _ => (x, y)        // No movement
      }

      // Calculate new snake position
      val newSnake: List[(Double, Double)] =
        // Reset snake if it hits wall or itself
        if (newx < 0 || newx >= 600 || newy < 0 || newy >= 600 || snake.tail.contains((newx, newy)))
          initialSnake
        // Grow snake if food is eaten
        else if (food == (newx, newy))
          food :: snake
        // Normal movement (remove tail, add new head)
        else
          (newx, newy) :: snake.init

      // Generate new food if current food was eaten
      val newFood =
        if (food == (newx, newy))
          randomFood()
        else
          food

      State(newSnake, newFood)
    }

    // Convert state to list of Rectangle objects for rendering
    def rectangles: List[Rectangle] = square(food._1, food._2, Red) :: snake.map {
      case (x, y) => square(x, y, Green)
    }
  }

  // Generate random food position on 24x24 grid
  def randomFood(): (Double, Double) =
    (Random.nextInt(24) * 25, Random.nextInt(24) * 25)

  // Helper function to create squares for snake segments and food
  def square(xr: Double, yr: Double, color: Color) = new Rectangle {
    x = xr
    y = yr
    width = 25
    height = 25
    fill = color
  }

  // Main game initialization and setup
  override def start(): Unit = {
    // Initialize game state properties
    val state = ObjectProperty(State(initialSnake, randomFood()))
    val frame = IntegerProperty(0)
    val direction = IntegerProperty(4) // right

    // Update state when frame changes
    frame.onChange {
      state.update(state.value.newState(direction.value))
    }


    // Set up the game window
    stage = new JFXApp3.PrimaryStage {
      width = 600
      height = 600
      scene = new Scene {
        fill = White
        content = state.value.rectangles
        // Handle keyboard input
        onKeyPressed = key => key.getText match {
          case "w" => direction.value = 1 // Up
          case "s" => direction.value = 2 // Down
          case "a" => direction.value = 3 // Left
          case "d" => direction.value = 4 // Right
        }

        // Update display when state changes
        state.onChange(Platform.runLater {
          content = state.value.rectangles
        })
      }
    }

    // Start the game loop
    gameLoop(() => frame.update(frame.value + 1))
  }

}