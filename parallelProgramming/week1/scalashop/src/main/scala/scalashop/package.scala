
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    if (radius == 0) src(x,y)
    else {
      // Get surrounding pixels, limited by picture height and width
      val pixels = (for {
        r <- 0 to radius
        newX <- (x - r) to (x + r)
        newY <- (y - r) to (y + r)
      } yield src(clamp(newX, 0, src.width - 1), clamp(newY, 0, src.height - 1))).toList.distinct

      // Get average pixel values
      val r = pixels.map(pixel => red(pixel)).foldLeft(0)((b, a) => a + b)
      val g = pixels.map(pixel => green(pixel)).foldLeft(0)((b, a) => a + b)
      val b = pixels.map(pixel => blue(pixel)).foldLeft(0)((b, a) => a + b)
      val a = pixels.map(pixel => alpha(pixel)).foldLeft(0)((b, a) => a + b)
      val count = pixels.size

      // Return concurrent tasks in rgba
      rgba(r / count, g / count, b / count, a / count)
    }
  }

}
