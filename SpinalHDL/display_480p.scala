package castledrawing
import spinal.core._
import spinal.lib._
class display_480p(
                   CORDW: Int = 16,  // signed coordinate width (bits)
                   H_RES: Int = 640, // horizontal resolution (pixels)
                   V_RES: Int = 480, // vertical resolution (lines)
                   H_FP: Int = 16,   // horizontal front porch
                   H_SYNC: Int = 96, // horizontal sync
                   H_BP: Int = 48,   // horizontal back porch
                   V_FP: Int = 10,   // vertical front porch
                   V_SYNC: Int = 2,  // vertical sync
                   V_BP: Int = 33,   // vertical back porch
                   H_POL: Boolean = false, // horizontal sync polarity (false: neg, true: pos)
                   V_POL: Boolean = false  // vertical sync polarity (false: neg, true: pos)
                 ) extends Component {
  noIoPrefix()
  val io = new Bundle {
    val clk_pix = in Bool()
    val rst_pix = in Bool()
    val hsync = out Bool()
    val vsync = out Bool()
    val de = out Bool()
    val frame = out Bool()
    val line = out Bool()
    val sx = out SInt (CORDW bits)
    val sy = out SInt (CORDW bits)
  }
  val myClockDomain = ClockDomain(
    clock = io.clk_pix,
    reset = io.rst_pix,
    config = ClockDomainConfig(
      resetActiveLevel = HIGH,
      resetKind = ASYNC
    )
  )
  val myArea = new ClockingArea(myClockDomain) {
    val H_STA = S(0 - H_FP - H_SYNC - H_BP, CORDW bits)
    val HS_STA = H_STA + H_FP
    val HS_END = HS_STA + H_SYNC
    val HA_STA = S(0, CORDW bits)
    val HA_END = S(H_RES - 1, CORDW bits)
    val V_STA = S(0 - V_FP - V_SYNC - V_BP, CORDW bits)
    val VS_STA = V_STA + V_FP
    val VS_END = VS_STA + V_SYNC
    val VA_STA = S(0, CORDW bits)
    val VA_END = S(V_RES - 1, CORDW bits)
    val x = Reg(SInt(CORDW bits)) init (H_STA)
    val y = Reg(SInt(CORDW bits)) init (V_STA)
    val hPolBool = if (H_POL) True else False
    val vPolBool = if (V_POL) True else False
    // Generate horizontal and vertical sync with correct polarity
    io.hsync := (hPolBool && x >= HS_STA && x < HS_END) || (!hPolBool && !(x >= HS_STA && x < HS_END))
    io.vsync := (vPolBool && y >= VS_STA && y < VS_END) || (!vPolBool && !(y >= VS_STA && y < VS_END))
    when(io.rst_pix) {
      io.hsync := hPolBool
      io.vsync := vPolBool
    }
    io.de := (y >= VA_STA && x >= HA_STA)
    io.frame := (y === V_STA && x === H_STA)
    io.line := (x === H_STA)
    when(io.rst_pix) {
      io.de := False
      io.frame := False
      io.line := False
    }
    when(x === HA_END) {
      x := H_STA
      y := Mux(y === VA_END, V_STA, y + 1)
    } otherwise {
      x := x + 1
    }
    when(io.rst_pix) {
      x := H_STA
      y := V_STA
    }
    io.sx := x
    io.sy := y
    when(io.rst_pix) {
      io.sx := H_STA
      io.sy := V_STA
    }
  }
}
object display_480p_Verilog extends App {
  Config.spinal.generateVerilog(new display_480p())
}