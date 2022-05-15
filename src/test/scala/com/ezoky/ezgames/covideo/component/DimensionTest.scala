/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension.*
import org.junit.Assert.*
import org.junit.Test

class DimensionTest {
  
  @Test def sizeTest(): Unit = {
    val s0 = SizeValue(0)
    assertEquals(s0, SizeValue.Zero)

    val s1 = SizeValue(10.0)    
    val s2 = SizeValue(-10.0)
    assertEquals(s2, s1)
    
    val s3 = (10.0 size)
    assertEquals(s3, s1)
    
  }

  @Test def toricPositionTest(): Unit = {

    val c00 = PositionValue(0, SizeValue(0), Geometry.Toric)
    assertEquals(c00, PositionValue.Zero)

    val c01 = PositionValue(0, SizeValue(1), Geometry.Toric)
    assertEquals(c01, PositionValue.Zero)

    val c10 = PositionValue(1, SizeValue(0), Geometry.Toric)
    assertEquals(c01, PositionValue.Zero)

    val s1 = 12 size
    val c1 = PositionValue(10, s1, Geometry.Toric)
    assertEquals(c1, PositionValue(10, 20.0 size, Geometry.Toric))
    val c2 = PositionValue(-10, s1, Geometry.Toric)
    assertEquals(c2, PositionValue(2, 20.0 size, Geometry.Toric))


    val c10_10 = PositionValue(10, 10.0 size, Geometry.Toric)
    assertNotEquals(c10_10, c1)
    assertEquals(c10_10, PositionValue.Zero)


    val c10_9 = PositionValue(10, 9 size, Geometry.Toric)
    assertNotEquals(c10_9, c1)
    assertEquals(c10_9, PositionValue(1, 9.0 size, Geometry.Toric))

  }

  @Test def positionUsingImplicits: Unit = {
    {
      given Geometry = Geometry.Toric
      given SizeValue = (10 size)

      assertEquals((3 position), (13 position))
      assertEquals((3 position), (23 position))
      assertEquals((3 position), (-7 position))
      assertEquals((10 position), (0 position))
    }
    
    {
      given Geometry = Geometry.Bounded
      given SizeValue = (10 size)

      assertEquals((3 position), (17 position))
      assertEquals((3 position), (23 position))
      assertEquals((3 position), (-3 position))
      assertEquals((3 position), (-17 position))
      assertEquals((3 position), (-23 position))
    }
  }

  @Test def durationTest: Unit = {
    val du1 = DurationValue(10)
    val du2 = DurationValue(-10)

    assertEquals(du1, du2)
  }

  @Test def speedTest: Unit = {
    val sp1 = SpeedValue(10)
    val sp2 = SpeedValue(30.0, 3 steps)

    assertEquals(sp1, sp2)
  }


  @Test def moveTest: Unit = {
    {
      given Geometry = Geometry.Toric
      {
        given SizeValue = (10 size)

        assertEquals((2 speed).on(0 position), (2 position))
        assertEquals((2 speed).on(1 position), (3 position))
        assertEquals((2 speed).on(9 position), (1 position))
        assertEquals((-2 speed).on(9 position), (7 position))
        assertEquals((-2 speed).on(1 position), (9 position))
        assertEquals((-2 speed).on(0 position), (8 position))
      }
      {
        given SizeValue = (6 size)

        assertEquals((2 speed).on(3 position), (5 position))
        assertEquals((2 speed).on(5 position), (1 position))
      }
    }
    {
      given Geometry = Geometry.Bounded
      {
        given SizeValue = (6 size)

        assertEquals((2 speed).on(0 position), (2 position))
        assertEquals((2 speed).on(1 position), (3 position))
        assertEquals((2 speed).on(9 position), (5 position))
        assertEquals((-2 speed).on(0 position), (2 position))
        assertEquals((-2 speed).on(1 position), (1 position))
        assertEquals((-2 speed).on(9 position), (1 position))
      }
    }
  }


  @Test def accelerateTest: Unit = {
    {
      given Geometry = Geometry.Toric
      {
        given SizeValue = (10 size)

        assertEquals((3 acceleration)(2 speed), (5 speed))
        assertEquals((3 acceleration)(2 speed).on(3 position), (8 position))
      }
      {
        given SizeValue = (6 size)

        assertEquals((3 acceleration)(2 speed).on(3 position), (2 position))
      }
    }
    {
      given Geometry = Geometry.Bounded
      {
        given SizeValue = (6 size)

        assertEquals((3 acceleration)(2 speed).on(3 position), (4 position))
      }
    }
  }
}
