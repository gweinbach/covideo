package com.ezoky.ezgames.covideo.system.swing

import org.junit.Assert.*
import org.junit.Test

/**
 * @author gweinbach on 04/01/2022
 * @since 0.2.0
 */
class AssetsTest {

  @Test def assetLoad: Unit = {
    val asset = Assets.SmileySunglasses
    assertNotNull(asset.getImage)
    assert(asset.getIconWidth > 0)
    assert(asset.getIconHeight > 0)
  }
}
