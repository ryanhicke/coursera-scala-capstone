package observatory.noncoursera

import observatory.{Color, Location, Temperature, Visualization}
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
      (46.0, Color(255, 128, 128)),
      (4.0, Color(85, 255, 170)),
      (-10.0, Color(0, 85, 255))
    )

    forAll(tests) { (temperature: Temperature, expected: Color) =>
      Visualization.interpolateColor(Visualization.colors, temperature) should be(expected)
    }
  }

  it should "return the largest color if the value is bigger than biggest known temp" in {
    Visualization.interpolateColor(List((79, Color(255, 255, 255))), 100.0) should be (Color(255, 255, 255))
  }

  it should "pass test using different scale" in {
    val scale = List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255)))
    Visualization.interpolateColor(scale, -0.75) should be (Color(191,0,64))
  }

}
