package observatory.noncoursera

import java.time.LocalDate

import observatory.fixtures.BaseSpec
import observatory.{StationId, StationTemperature, Temperature}
import org.scalatest.prop.TableDrivenPropertyChecks._

class ModelsSpec extends BaseSpec {
  val validTolerance = 0.01
  def almostEqual(d1: Double, d2: Double, epsilon: Double = validTolerance) = math.abs(d1 - d2) <= epsilon

  "StationTemperature" should "return temperature in celsius" in {
    val stationId = StationId(Some("1"), None)
    val localDate = LocalDate.of(2018, 2, 1)

    val temperatures = Table(
      ("fahrenheit", "celsius"),
      (-50.0, -45.56),
      (-40.0, -40.0),
      (-20.0, -28.89),
      (0.0, -17.78),
      (20.0, -6.67),
      (32.0, 0.0),
      (40.0, 4.44),
      (60.0, 15.56),
      (80.0, 26.67),
      (100.0, 37.78),
      (120.0, 48.89)
    )

    forAll(temperatures) { (f: Temperature, c: Temperature) =>
      StationTemperature(stationId, localDate, f).tempCelsius should be(c +- validTolerance)
    }
  }
}
