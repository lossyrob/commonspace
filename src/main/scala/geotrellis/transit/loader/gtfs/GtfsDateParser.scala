package geotrellis.transit.loader.gtfs

import geotrellis.transit.Logger
import geotrellis.network.{Time, Duration, Location}
import geotrellis.network.graph._
import geotrellis.network._
import com.github.nscala_time.time.Imports._
import com.azavea.gtfs._
import com.azavea.gtfs.data._

import scala.collection.mutable

object GtfsDateParser {
  val gtfsTimeRegex = """(\d?\d):(\d\d):(\d\d)""".r

  def parse(name: String, gtfsDirectory: String, date: LocalDate): (MutableGraph, NamedLocations) = {
    val data = GtfsData.fromFile(gtfsDirectory)
    import data.context._

    def getVertex(stop: Stop, stopsToVertices: mutable.Map[Stop, Vertex], graph: MutableGraph) =
      if(stopsToVertices.contains(stop)) {
        stopsToVertices(stop)
      } else {
        val v = StationVertex(Location(stop.stop_lon, stop.stop_lat), stop.stop_name)
        stopsToVertices(stop) = v
        graph += v
        v
      }

    def setEdges(trip: Trip, stopsToVertices: mutable.Map[Stop, Vertex], service: String, graph: MutableGraph): Int = {
      var count = 0
      trip.stopTimes
        .sortBy(_.stop_sequence)
        .reduce { (departing, arriving) =>
          val departingStop = departing.getStop.getOrElse { sys.error(s"Invalid stop id: ${departing.stop_id}") }
          val arrivingStop = departing.getStop.getOrElse { sys.error(s"Invalid stop id: ${arriving.stop_id}") }
          val departingVertex = getVertex(departingStop, stopsToVertices, graph)
          val arrivingVertex = getVertex(arrivingStop, stopsToVertices, graph)

          graph
            .edges(departingVertex)
            .addEdge(
              TransitEdge(
                arrivingVertex,
                service,
                Time(departing.departure_time.toStandardDuration.getStandardSeconds.toInt),
                geotrellis.network.Duration(arriving.timeTo(departing).toStandardDuration.getStandardSeconds.toInt)
              )
            )
          count += 1
          arriving
        }
      count
    }

    val g = MutableGraph()

    val (edges, namedLocations) =
      data.service.foldLeft( (0, NamedLocations.EMPTY) ) { (result, service) =>
        val trips = service.getTripsOn(date)
        

        val stopsToVertices = mutable.Map[Stop, Vertex]()

        val edges = Logger.timedCreate("Creating edges for trips...", "Done creating edges.") { () =>
          trips.map(setEdges(_, stopsToVertices, name, g)).foldLeft(0)(_ + _)
        }

        val namedLocations =
          NamedLocations(
            for( vertex <- stopsToVertices.values) yield {
              NamedLocation(vertex.name, vertex.location)
            }
          )

        (result._1 + edges, result._2.mergeIn(namedLocations))
      }

    Logger.log(s"$edges edges set.")
    (g, namedLocations)
  }
}
