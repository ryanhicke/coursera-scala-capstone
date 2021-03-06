package observatory

import java.time.LocalDate

import com.sksamuel.scrimage.RGBColor

/**
  * Introduced in Week 1. Represents a location on the globe.
 *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {
  def isAntipode(that: Location): Boolean = {
    (that.lat == lat * -1) && (that.lon == (180 - math.abs(lon)) * (if (0 <= lon) -1 else 1))
  }
}

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int)

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int)

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  * @param red Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int) {
  def toRGBColor: RGBColor = RGBColor(red, green, blue)
}

// ***** above are coursera defined classes whose parameters cannot be modified
// ***** below are user defined classes whose parameters can be modified.

case class StationId(stn: Option[String], wban: Option[String])

case class Station(stationId: StationId, location: Location)

case class StationTemperature(stationId: StationId, localDate: LocalDate, tempFahrenheit: Temperature) {
  private def convertToFahrenheitToCelsius(tempFarenheit: Temperature) = (tempFarenheit - 32) * (5.0/9.0)

  val tempCelsius = convertToFahrenheitToCelsius(tempFahrenheit)
}

