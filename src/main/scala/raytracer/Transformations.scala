// Copyright (C) 2019 James McMurray
//
// raytracer_challenge is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// raytracer_challenge is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with raytracer_challenge  If not, see <http://www.gnu.org/licenses/>.

package raytracer

import cats.implicits._

object Translation {
  def apply(x: Double, y: Double, z: Double): Matrix = {
    new Matrix(
      Matrix.getIdentityMatrix(4).m.zipWithIndex.map(
        (a: (Array[Double], Int)) => (a._1.zipWithIndex.map(
          (b: (Double, Int)) => a._2 match {
            case 0 => if (b._2 === 3) x else b._1
            case 1 => if (b._2 === 3) y else b._1
            case 2 => if (b._2 === 3) z else b._1
            case 3 => if (b._2 === 3) 1 else b._1
          }
        )
          )
      )
    )
  }
}

object Scaling {
  def apply(x: Double, y: Double, z: Double): Matrix = {
    new Matrix(
      Matrix.getIdentityMatrix(4).m.zipWithIndex.map(
        (a: (Array[Double], Int)) => (a._1.zipWithIndex.map(
          (b: (Double, Int)) => a._2 match {
            case 0 => if (b._2 === 0) x else b._1
            case 1 => if (b._2 === 1) y else b._1
            case 2 => if (b._2 === 2) z else b._1
            case 3 => if (b._2 === 3) 1 else b._1
          }
        )
          )
      )
    )
  }
}

object RotationX {
  def apply(r: Double): Matrix = {
    new Matrix(
      Matrix.getIdentityMatrix(4).m.zipWithIndex.map(
        (a: (Array[Double], Int)) => (a._1.zipWithIndex.map(
          (b: (Double, Int)) => a._2 match {
            case 1 => b._2 match {
              case 1 => math.cos(r)
              case 2 => -1 * math.sin(r)
              case _ => b._1
            }
            case 2 => b._2 match {
              case 1 => math.sin(r)
              case 2 => math.cos(r)
              case _ => b._1
            }
            case _ => b._1
          }
        )
          )
      )
    )
  }
}

object RotationY {
  def apply(r: Double): Matrix = {
    new Matrix(
      Matrix.getIdentityMatrix(4).m.zipWithIndex.map(
        (a: (Array[Double], Int)) => (a._1.zipWithIndex.map(
          (b: (Double, Int)) => a._2 match {
            case 0 => b._2 match {
              case 0 => math.cos(r)
              case 2 => math.sin(r)
              case _ => b._1
            }
            case 2 => b._2 match {
              case 0 => -1 * math.sin(r)
              case 2 => math.cos(r)
              case _ => b._1
            }
            case _ => b._1
          }
        )
          )
      )
    )
  }
}

object RotationZ {
  def apply(r: Double): Matrix = {
    new Matrix(
      Matrix.getIdentityMatrix(4).m.zipWithIndex.map(
        (a: (Array[Double], Int)) => (a._1.zipWithIndex.map(
          (b: (Double, Int)) => a._2 match {
            case 0 => b._2 match {
              case 0 => math.cos(r)
              case 1 => -math.sin(r)
              case _ => b._1
            }
            case 1 => b._2 match {
              case 0 => math.sin(r)
              case 1 => math.cos(r)
              case _ => b._1
            }
            case _ => b._1
          }
        )
          )
      )
    )
  }
}

object Shearing {
  def apply(xy: Double, xz: Double, yx: Double, yz: Double, zx: Double, zy: Double): Matrix = {
    new Matrix(
      Matrix.getIdentityMatrix(4).m.zipWithIndex.map(
        (a: (Array[Double], Int)) => (a._1.zipWithIndex.map(
          (b: (Double, Int)) => a._2 match {
            case 0 => b._2 match {
              case 1 => xy
              case 2 => xz
              case _ => b._1
            }
            case 1 => b._2 match {
              case 0 => yx
              case 2 => yz
              case _ => b._1
            }
            case 2 => b._2 match {
              case 0 => zx
              case 1 => zy
              case _ => b._1
            }
            case _ => b._1
          }
        )
          )
      )
    )
  }
}

object viewTransform {
  def apply(from: RTTuple, to: RTTuple, up: RTTuple): Matrix = {
    val forward: RTTuple = (to - from).normalise()
    val upn: RTTuple = up.normalise()
    val left: RTTuple = forward.cross(upn)
    val true_up: RTTuple = left.cross(forward)

    val orientation: Matrix = new Matrix(Array(left.toArray, true_up.toArray, forward.negate().toArray, Point(0, 0, 0).toArray))

    orientation * Translation(-from.x, -from.y, -from.z)
  }
}