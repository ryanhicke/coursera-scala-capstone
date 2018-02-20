package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  val EARTH_RADIUS_KM = 6371.0

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    inverseDistanceWeightedTemperature(temperatures, location)
  }

  def greatCircleDistance(loc1: Location, loc2: Location): Double = {
    if (loc1 == loc2)
      0.0
    else if (loc1.isAntipode(loc2))
      math.Pi * EARTH_RADIUS_KM
    else {
      val latDistance = (loc2.lat - loc1.lat).toRadians
      val lonDistance = (loc2.lon - loc1.lon).toRadians

      val a = math.pow(math.sin(latDistance / 2), 2) +
        math.pow(math.sin(lonDistance / 2), 2) *
          math.cos(loc1.lat.toRadians) *
          math.cos(loc2.lat.toRadians)
      val c = 2 * math.asin(math.sqrt(a))
      c * EARTH_RADIUS_KM
    }
  }

  def inverseDistanceWeightedTemperature(
      temperatures: Iterable[(Location, Temperature)],
      location: Location,
      p: Int = 2): Temperature = {
    val (vals, total) = temperatures.foldLeft((0.0, 0.0)) { case ((weightedSum, weightedTotals), (loc, temp)) =>
      val weight = 1 / math.pow(greatCircleDistance(location, loc), p)
      (weightedSum + (weight * temp), weightedTotals + weight)
    }
    vals / total
  }

  val colors: List[(Temperature, Color)] = List[(Temperature, Color)](
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 108)),
    (-60.0, Color(0, 0, 0))
  )

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val highestColor = points.toList.reverse.head
    val lowestColor = points.head
    def getNearestTwo = {
      val filteredLower = points.filter{ case (t, _) => t <= value}
      val filteredHigher = points.filter{ case (t, _) => value <= t}
      if (filteredLower.isEmpty)
        (lowestColor, lowestColor)
      else if(filteredHigher.isEmpty)
        (highestColor, highestColor)
      else {
        (
          filteredLower.reduce((l, r) => if (l._1 < r._1) r else l),
          filteredHigher.reduce((l, r) => if (l._1 < r._1) l else r)
        )
      }
    }

    val filtered = points.filter{ case (t, _) => t == value}
    if (filtered.nonEmpty)
      filtered.head._2
    else {
      val (l, r) = getNearestTwo
      if (l._1 == r._1)
        l._2
      else {
        // compute linear interpolation
        Color(
          linearInterpolation(l._1, l._2.red.toDouble, r._1, r._2.red.toDouble, value),
          linearInterpolation(l._1, l._2.green.toDouble, r._1, r._2.green.toDouble, value),
          linearInterpolation(l._1, l._2.blue.toDouble, r._1, r._2.blue.toDouble, value)
        )
      }
    }
  }

  //solve for y2
  private def linearInterpolation(x1: Double, y1: Double, x3: Double, y3: Double, x2: Double): Int =
    math.round(((x2 - x1) * (y3 - y1) / (x3 - x1)) + y1).toInt

  val WIDTH = 360
  val HEIGHT = 180

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    Image(WIDTH, HEIGHT, locations.map{ location =>
      val temperature = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, temperature)
      Pixel(color.toRGBColor)
    }.toArray)
  }

  def locations = {
    val topLeft = Location(90, -180)
    (0 until 380).flatMap { x => (0 until 180).map { y => Location(topLeft.lat - y, topLeft.lon + x) } }.toList
  }
}

