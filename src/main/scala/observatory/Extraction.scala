package observatory

import java.time.LocalDate

import scala.util.Try

import observatory.utils.SparkUtils
import org.apache.spark.rdd.RDD

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    locateTemperaturesRDD(year, stationsFile, temperaturesFile).collect
  }

  private def locateTemperaturesRDD(year: Year, stationsFile: String, temperaturesFile: String) = {
    val groupedStations: RDD[(StationId, Iterable[Station])] = loadStationsFile(stationsFile).groupBy(_.stationId)
    val groupedTemperatures: RDD[(StationId, Iterable[StationTemperature])] =
      loadTemperaturesFile(year, temperaturesFile).groupBy(_.stationId)
    val joinedRDD: RDD[(StationId, (Iterable[Station], Iterable[StationTemperature]))] =
      groupedStations.join(groupedTemperatures)
    joinedRDD.flatMapValues { case (stations, temperatures) =>
      val stationHead = stations.head
      temperatures.map { case temp  =>
        (temp.localDate, stationHead.location, temp.tempCelsius)
      }
    }.values
  }

  private def stringToOption[T](str: String): Option[T] =
    if (str.trim.nonEmpty)
      Some(str.trim.asInstanceOf[T])
    else
      None

  def loadStationsFile(stationsFile: String): RDD[Station] = {
    SparkUtils.loadResourceAsTextFile(stationsFile).flatMap { row =>
      val split = row.split(",", -1).map(_.trim)
      val stationId = StationId(stringToOption[String](split(0)), stringToOption[String](split(1)))
      val location =
        if (split(2).isEmpty || split(3).isEmpty) None
        else Some(Location(split(2).toDouble, split(3).toDouble))
      //do not include stations without location data
      location match {
        case Some(l) => Some(Station(stationId, l))
        case None => None
      }
    }
  }

  def loadTemperaturesFile(year: Year, temperaturesFile: String): RDD[StationTemperature] = {
    SparkUtils.loadResourceAsTextFile(temperaturesFile).flatMap { row =>
      try {
        val split = row.split(",", -1).map(_.trim)
        val stationId = StationId(stringToOption[String](split(0)), stringToOption[String](split(1)))
        val localDate = LocalDate.of(year, split(2).toInt, split(3).toInt)
        Some(StationTemperature(stationId, localDate, split(4).toDouble))
      } catch {
        case _: Throwable => None
      }
    }
  }


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val groupedByLocation = SparkUtils.spark.parallelize(records.toSeq).groupBy(_._2)
    groupedByLocation.mapValues{ locationRecords =>
      locationRecords.foldLeft(0.0){ case (agg, record) => agg + record._3 } / locationRecords.size.toDouble
    }.collect
  }

}
