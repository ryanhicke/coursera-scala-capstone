package observatory.noncoursera

import java.time.LocalDate

import observatory._
import observatory.fixtures.BaseSpec


class ExtractionSpec extends BaseSpec {
  def convertToFahrenheitToCelsius(tempFarenheit: Temperature) = (tempFarenheit - 32) * (5.0/9.0)

  "Extraction" should "extract a stations file" in {
    val stations = Extraction.loadStationsFile("/small_stations.csv")
    val collected = stations.collect()
    collected.length should be(2)
    collected should be(Array(
      Station(StationId(Some("724017"), Some("03707")), Location(37.358, -78.438)),
      Station(StationId(Some("724017"), None), Location(37.350, -78.433))
    ))
  }

  it should "extract a temperature file" in {
    val temperatures = Extraction.loadTemperaturesFile(2016, "/small_temps.csv").collect()
    temperatures.length should be(4)
    temperatures should be(Array(
      StationTemperature(StationId(Some("010013"), None), LocalDate.of(2016, 11, 25), 39.2),
      StationTemperature(StationId(Some("724017"), None), LocalDate.of(2016, 8, 11), 81.14),
      StationTemperature(StationId(Some("724017"), Some("03707")), LocalDate.of(2016, 12, 6), 32.0),
      StationTemperature(StationId(Some("724017"), Some("03707")), LocalDate.of(2016, 1, 29), 35.6)
    ))
  }

  it should "associate temperatures with locations" in {

    val values = Extraction.locateTemperatures(2016, "/small_stations.csv", "/small_temps.csv")
    values.size should be(3)
    values.toArray should be(Array(
      (LocalDate.of(2016, 8, 11), Location(37.350, -78.433), convertToFahrenheitToCelsius(81.14)),
      (LocalDate.of(2016, 12, 6), Location(37.358, -78.438), convertToFahrenheitToCelsius(32.0)),
      (LocalDate.of(2016, 1, 29), Location(37.358, -78.438), convertToFahrenheitToCelsius(35.6))
    ))
  }

  it should "calculate average yearly records" in {
    val values = Extraction.locationYearlyAverageRecords(Array(
      (LocalDate.of(2016, 8, 11), Location(37.350, -78.433), 23.0),
      (LocalDate.of(2016, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2016, 1, 29), Location(37.358, -78.438), 1.0)
    ))
    values should be(Array(
      (Location(37.358, -78.438), 0.5),
      (Location(37.350, -78.433), 23.0)
    ))
  }
}
