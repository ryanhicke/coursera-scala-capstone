package observatory.noncoursera

import observatory.Location
import observatory.fixtures.BaseSpec
import observatory.Visualization.{EARTH_RADIUS_KM, greatCircleDistance}
import org.scalatest.prop.TableDrivenPropertyChecks._

class GreatCircleDistanceSpec extends BaseSpec {
  val sphericalEarthError = .03
  "greatCircleDistance" should "return 0 for the same point" in {
    (-1000 to 1000).foreach(lat => (-1000 to 1000).foreach { lon =>
      greatCircleDistance(Location(lat, lon), Location(lat, lon)) should be(0.0)
    })
  }

  it should "return Pi for the antipode" in {
    val tests = Table(
      ("location1", "location2"),
      (Location(35.0,90.0), Location(-35.0,-90.0)),
      (Location(40.0, 117.0), Location(-40.0, -63.0)),
      (Location(-27.5, 135.5), Location(27.5, -44.5)),
      (Location(21.0, -158.0), Location(-21.0, 22.0))
    )
    forAll(tests) { (loc1: Location, loc2: Location) =>
      greatCircleDistance(loc1, loc2) should be(math.Pi * EARTH_RADIUS_KM)
    }
  }

  it should "calculate the Great Circle Distance using in other cases" in {
    val tests = Table(
      ("location1", "location2", "greatCircleDistance"),
      (Location(50, 50), Location(20, 20), 4255.0),
      (Location(-50, 20), Location(-100, 20), 5560.0),
      (Location(-50, 20), Location(-100, -20), 5339.0),
      (Location(174, 11), Location(-100, -20), 9720.0)
    )

    forAll(tests) { (loc1: Location, loc2: Location, expected: Double) =>
      val epsilon = expected * sphericalEarthError
      greatCircleDistance(loc1, loc2) should be(expected +- epsilon)
    }
  }
}
