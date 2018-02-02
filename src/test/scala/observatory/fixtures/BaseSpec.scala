package observatory.fixtures

import org.scalatest._

class BaseSpec extends FlatSpecLike
  with GivenWhenThen
  with BeforeAndAfterEach
  with BeforeAndAfterAll
  with Matchers {

}
