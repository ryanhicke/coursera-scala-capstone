package observatory.noncoursera

import observatory.{Color, Temperature, Visualization}
import observatory.fixtures.BaseSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

class InterpolateColorSpec extends BaseSpec {
  "InterpolateColor" should "get exact values" in {
    val tests = Table(
      ("temperature", "expected"),
      (60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 108)),
      (-60.0, Color(0, 0, 0))
    )

    forAll(tests) { (temperature: Temperature, expected: Color) =>
      Visualization.interpolateColor(Visualization.colors, temperature) should be(expected)
    }
  }

  it should "get interpolated values" in {
    val tests = Table(
      ("temperature", "expected"),
      (46.0, Color(255, 127, 127)),
      (4.0, Color(85, 255, 170)),
      (-10.0, Color(0, 85, 255))
    )

    forAll(tests) { (temperature: Temperature, expected: Color) =>
      Visualization.interpolateColor(Visualization.colors, temperature) should be(expected)
    }
  }
}
