package geotrellis.network.graph

import geotrellis.network._
import com.github.nscala_time.time.{Imports => joda}

sealed abstract class EdgeType

case object WalkEdge extends EdgeType {
  def apply(target: Vertex, travelTime: Duration) = 
    Edge(target, Time.ANY, travelTime, Walking)
}

case object TransitEdge extends EdgeType {
  def apply(target: Vertex,
            service: String,
            time: Time,
            travelTime: Duration,
            weeklySchedule: WeeklySchedule): Edge =
    Edge(target, time, travelTime, ScheduledTransit(service, weeklySchedule))

  def apply(target: Vertex,
            service: String,
            time: Time,
            travelTime: Duration): Edge =
    apply(target, service, time, travelTime, EveryDaySchedule)

  def apply(target: Vertex,
            service: String,
            time: joda.LocalDateTime,
            travelTime: joda.Duration): Edge =
    ???

  def apply(target: Vertex,
            service: String,
            time: joda.LocalDateTime,
            travelTime: joda.Duration,
            weeklySchedule:WeeklySchedule): Edge =
    ???
}

case object BikeEdge extends EdgeType {
  def apply(target: Vertex, travelTime: Duration) = 
    Edge(target, Time.ANY, travelTime, Biking)
}

case class Edge(target: Vertex,
                time: Time,
                travelTime: Duration,
                mode: TransitMode) {
  def isAnyTime = time.isAny
}
