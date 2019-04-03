package com.rlucas

import com.rlucas.db.Database
import com.rlucas.models.Parsing._
import scalaj.http.Http

object Main extends App with Database {

  val url = "https://calendar.google.com/calendar/ical/8hi4kdfj3fg66nflut86f3rncc%40group.calendar.google.com/private-d1e99dcebf3441a1139ca70e907d6160/basic.ics"
  val response = Http(url).asString.body
  val rows: List[String] = response.split('\n').map(_.trim.filter(_ >= ' ')).toList
  println(parseCalendar(rows, false))

}
