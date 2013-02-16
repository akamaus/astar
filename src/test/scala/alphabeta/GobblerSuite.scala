package alphabeta

import org.scalatest.FunSuite
import com.sciolizer.alphabeta._
import com.sciolizer.alphabeta.State
import com.sciolizer.alphabeta.Square

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/16/13
 * Time: 9:44 AM
 */
class GobblerSuite extends FunSuite {

  val blank: State = new GobblerGame().getInitialState

  test("Terminal exists") {
    val gg = new GobblerGame()
//    val state = blank.copy(board = blank.board ++ Map((0, 0) -> Square(List(Piece(0, 0, 0))),
//                          (0, 1) -> Square(List(Piece(0, 0, 1))),
//                          (0, 2) -> Square(List(Piece(0, 1, 0)))))
//    assert(gg.isTerminal(state))
    assert(false)
  }

  test("Terminal reachable") {
    val gg = new GobblerGame()
    var state = gg.getInitialState
    val actions: List[Action] = List(
      Action(Piece(0, 0, 0), (0, 0)),
      Action(Piece(1, 0, 0), (0, 1)),
      Action(Piece(0, 0, 1), (1, 0)),
      Action(Piece(1, 0, 1), (1, 1)),
      Action(Piece(0, 1, 0), (2, 0))
    )
    val fstate = actions.foldLeft(gg.getInitialState)((s, act) => gg.getResult(s, act))
    assert(gg.isTerminal(fstate))
  }

}
