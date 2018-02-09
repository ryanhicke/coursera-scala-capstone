package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  val EARTH_RADIUS_KM=6371.0

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    ???
  }

  def greatCircleDistance(loc1: Location, loc2: Location): Double = {
    if (loc1 == loc2)
      0.0
    else if(loc1.isAntipode(loc2))
      math.Pi * EARTH_RADIUS_KM
    else {
      val latDistance = (loc2.lat - loc1.lat).toRadians
      val lonDistance = (loc2.lon - loc1.lon).toRadians

      val a = math.pow(math.sin(latDistance/2), 2) +
        math.pow(math.sin(lonDistance/2), 2) *
        math.cos(loc1.lat.toRadians) *
        math.cos(loc2.lat.toRadians)
      val c = 2 * math.asin(math.sqrt(a))
      c * EARTH_RADIUS_KM
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    ???
  }

}

