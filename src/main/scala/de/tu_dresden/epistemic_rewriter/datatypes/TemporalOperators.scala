package de.tu_dresden.epistemic_rewriter.datatypes

import java.sql.Timestamp
import java.time.format.DateTimeParseException
import java.time.{Duration, LocalDateTime}

import org.joda.time.Period
import slick.jdbc.PostgresProfile.api._

import scala.collection.{SortedSet, mutable}


object TimePoint {
  def parse(s: String) = {
    try {
      TimePoint(LocalDateTime.parse(s))
    } catch {
      case e: DateTimeParseException => throw e
    }
  }

  implicit def ordered: Ordering[TimePoint] = new Ordering[TimePoint] {
    def compare(x: TimePoint, y: TimePoint): Int = x.p compareTo y.p
  }

  implicit def timePointToLocalDateTime(t: TimePoint): LocalDateTime = t.p

  implicit def localDateTimeToTimePoint(p: LocalDateTime) = TimePoint(p)

  implicit def timePointToExtTimePoint(t: TimePoint): ExtTimePoint = Point(t.p)

  implicit def localDateTimeToExtTimePoint(t: LocalDateTime): ExtTimePoint = Point(t)

}

case class TimePoint(p: LocalDateTime) {
  override def toString: String = p.toString()
  def toInterval:AbsInterval = AbsInterval(Point(this), Point(this))
}


trait TimeAware {
  /*implicit def ordered: Ordering[Timepoint] = new Ordering[Timepoint] {
    def compare(x: Timepoint, y: Timepoint): Int = x compareTo y
  }*/

  implicit def ordered: Ordering[ExtTimePoint] = new Ordering[ExtTimePoint] {
    def compare(x: ExtTimePoint, y: ExtTimePoint): Int = (x, y) match {
      case (NegInfinity(), _) => -1
      case (_, NegInfinity()) => 1
      case (_, PosInfinity()) => 1
      case (PosInfinity(), _) => -1
      case (Point(a), Point(b)) => a compareTo b
    }
  }
}

/**
  * An Extended Period that can also represent +/- infinity values
  */
object ExtPeriod {
  implicit def fromPeriod(p: Period): ExtPeriod = FinitePeriod(p)
}

trait ExtPeriod
case class FinitePeriod(p: Period) extends ExtPeriod {
  override def toString: String = p.toString
}
case class PosInfinitePeriod() extends ExtPeriod {
  override def toString: String = "∞"
}

case class NegInfinitePeriod() extends ExtPeriod {
  override def toString: String = "-∞"
}


/**
  * An Extended Timepoint that can also represent +/- infinity values
  */
object ExtTimePoint {
  def parse(s: String) = s match {
    case "-infinity" => NegInfinity()
    case "infinity" => PosInfinity()
    case _ => Point(TimePoint.parse(s))
  }
}



trait ExtTimePoint

case class NegInfinity() extends ExtTimePoint {
  override def toString() = "-infinity"
}

case class PosInfinity() extends ExtTimePoint {
  override def toString() = "infinity"
}

case class Point(t: TimePoint) extends ExtTimePoint {
  override def toString() = t.toString
}


trait TimeAwareStorage {
  /*implicit val localDateTimeToDateTime = MappedColumnType.base[LocalDateTime, Timestamp](
    l => Timestamp.valueOf(l),
    d => d.toLocalDateTime
  )*/
  implicit val timePointToDateTime = MappedColumnType.base[TimePoint, Timestamp](
    l => Timestamp.valueOf(l),
    d => d.toLocalDateTime
  )

  implicit val extTimePointToDateTime = MappedColumnType.base[ExtTimePoint, String](
    dt => dt.toString,
    s => ExtTimePoint.parse(s)
  )

  implicit def ordered: Ordering[LocalDateTime] = new Ordering[LocalDateTime] {
    def compare(x: LocalDateTime, y: LocalDateTime): Int = x compareTo y
  }
}

trait DiamondStorage {
  implicit val diamondToString = MappedColumnType.base[Diamond, String](
    l => l match {
      case l1: BoundedConvex => s"${l1.identifier} ${l1.bound.toString}"
      case _ => l.identifier
    },
    s => {
      val pattern = "([A-Z]^3) (.*)".r
      val m = pattern.findAllIn(s).matchData.next()
      m.group(1) match {
        case "---" => Diamond.MIN_CONVEX
        case "CON" => Convex()
        case "INC" => Increasing()
        case "DEC" => Decreasing()
        case "RIG" => Rigid()
        case "BCX" => BoundedConvex(Duration.parse(m.group(2)))
        case _ => throw new Exception("This should never happen..")
      }
    }

  )
}


object Diamond {
  val bounded_convex = raw"convex (.*)".r

  def fromString(s: String): Diamond = s match {
    case "convex" => Convex()
    case "increasing" => Increasing()
    case "decreasing" => Decreasing()
    case "rigid" => Rigid()
    case bounded_convex(d) => BoundedConvex(Duration.parse(d))
  }

  val MIN_DIFF = Duration.ZERO.plusMillis(1)
  val MIN_CONVEX = BoundedConvex(MIN_DIFF)

  def sup(d1: Diamond, d2: Diamond): Diamond = (d1, d2) match {
    case (_, Rigid()) | (Rigid(), _) => Rigid()
    case (Increasing(), Decreasing()) | (Decreasing(), Increasing()) => Rigid()
    case (Increasing(), _) | (_, Increasing()) => Increasing()
    case (Decreasing(), _) | (_, Decreasing()) => Decreasing()
    case (Convex(), _) | (_, Convex()) => Convex()
    case (BoundedConvex(b1), BoundedConvex(b2)) => if (b1.compareTo(b2) > 0) BoundedConvex(b1) else BoundedConvex(b2)
    /*case (BoundedConvex(b1), NoDiamond()) =>BoundedConvex(b1)
    case (NoDiamond(), BoundedConvex(b1)) => BoundedConvex(b1)
    case (_, _) => NoDiamond()*/
  }

  def inf(d1: Diamond, d2: Diamond): Diamond = (d1, d2) match {
    //case (NoDiamond(), _) | (_, NoDiamond()) => NoDiamond()
    case (BoundedConvex(b1), BoundedConvex(b2)) => if (b1.compareTo(b2) > 0) BoundedConvex(b2) else BoundedConvex(b1)
    case (BoundedConvex(b1), _) => BoundedConvex(b1)
    case (_, BoundedConvex(b1)) => BoundedConvex(b1)
    case (Convex(), _) | (_, Convex()) => Convex()
    case (Increasing(), Decreasing()) | (Decreasing(), Increasing()) => Convex()
    case (Increasing(), _) | (_, Increasing()) => Increasing()
    case (Decreasing(), _) | (_, Decreasing()) => Decreasing()
    case (Rigid(), _) | (_, Rigid()) | (_, _) => Rigid()
  }
}


trait Diamond extends TimeAware {
  def identifier: String

  override def toString() = identifier

  def complete(timepoints: SortedSet[TimePoint])(implicit availableTimepoints: SortedSet[TimePoint]): SortedSet[TimePoint]
}

case class Rigid() extends Diamond {
  override def identifier = "RIG"

  override def complete(timestamps: SortedSet[TimePoint])(implicit availableTimepoints: SortedSet[TimePoint]): SortedSet[TimePoint] = timestamps.isEmpty match {
    case true => timestamps
    case false => availableTimepoints
  }
}

case class Increasing() extends Diamond {
  override def identifier() = "INC"

  override def complete(timestamps: SortedSet[TimePoint])(implicit availableTimepoints: SortedSet[TimePoint]): SortedSet[TimePoint] = timestamps.isEmpty match {
    case true => timestamps
    case false => {
      val m = timestamps.min
      availableTimepoints.filter(_.isAfter(m)).+(m)
    }
  }
}

case class Decreasing() extends Diamond {
  override def identifier() = "DEC"

  override def complete(timestamps: SortedSet[TimePoint])(implicit availableTimepoints: SortedSet[TimePoint]): SortedSet[TimePoint] = timestamps.isEmpty match {
    case true => timestamps
    case false => {
      val m = timestamps.max
      availableTimepoints.filter(_.isBefore(m)).+(m)
    }
  }
}

case class Convex() extends Diamond {
  override def identifier() = "CON"

  override def complete(timestamps: SortedSet[TimePoint])(implicit availableTimepoints: SortedSet[TimePoint]): SortedSet[TimePoint] = timestamps.isEmpty match {
    case true => timestamps
    case false => {
      val a = timestamps.min
      val b = timestamps.max
      availableTimepoints.filter(t => t.isAfter(a) && t.isBefore(b)).+(a).+(b)
    }
  }
}

case class BoundedConvex(bound: Duration) extends Diamond {
  override def identifier: String = "BCX"

  override def toString() = s"${identifier}{${bound}}"

  override def complete(timestamps: SortedSet[TimePoint])(implicit availableTimepoints: SortedSet[TimePoint]): SortedSet[TimePoint] = timestamps.size match {
    case 0 | 1 => timestamps
    case _ => {
      val results: mutable.MutableList[TimePoint] = mutable.MutableList[TimePoint](timestamps.toSeq: _*)
      val ts = timestamps.toList
      for (i <- 0 to ts.size - 2) {
        val a = ts(i)
        val b = ts(i + 1)
        val delta = java.time.Duration.between(a.p, b.p)
        if (delta.compareTo(bound) <= 0) {
          results.++=(availableTimepoints.filter(t => t.isAfter(a) && t.isBefore(b)))
        }
      }
      SortedSet[TimePoint](results: _*)
    }
  }
}