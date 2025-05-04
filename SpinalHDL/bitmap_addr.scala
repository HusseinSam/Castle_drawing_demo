package castledrawing
import spinal.core._
class bitmap_addr(CORDW: Int = 16,  // signed coordinate width (bits)
                  ADDRW: Int = 16   // address width (bits)
                 ) extends Component {
  noIoPrefix()
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val bmpw = in SInt (CORDW bits) // bitmap width
    val bmph = in SInt (CORDW bits) // bitmap height
    val x = in SInt (CORDW bits) // horizontal pixel coordinate
    val y = in SInt (CORDW bits) // vertical pixel coordinate
    val offx = in SInt (CORDW bits) // horizontal offset
    val offy = in SInt (CORDW bits) // vertical offset
    val addr = out UInt (ADDRW bits) // pixel memory address
    val clip = out Bool() // pixel coordinate outside bitmap
  }
  val myClockDomain = ClockDomain(clock = io.clk, reset = io.rst, config = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC))
  val myArea = new ClockingArea(myClockDomain) {
    val addr_y1 = Reg(SInt(CORDW bits))
    val addr_x1 = Reg(SInt(CORDW bits))
    val addr_x2 = Reg(SInt(CORDW bits))
    val addr_mul = Reg(UInt(ADDRW bits))
    val addr_reg = Reg(UInt(ADDRW bits)) // Register for delayed output
    val clip_t1 = Reg(Bool())
    // Step 1
    addr_y1 := io.y + io.offy
    addr_x1 := io.x + io.offx
    // Step 2
    addr_mul := (io.bmpw * addr_y1).asUInt.resize(ADDRW)
    addr_x2 := addr_x1
    clip_t1 := (addr_x1 < 0 || addr_x1 > (io.bmpw-1)) || (addr_y1 < 0 || addr_y1 > (io.bmph-1))
    // Step 3
    val clip_t1_delayed = RegNext(clip_t1) // Delay clip_t1 by one cycle
    io.clip := clip_t1_delayed
    addr_reg := addr_mul + addr_x2.asUInt.resize(ADDRW)
    io.addr := addr_reg
  }
}
object bitmap_addr_Verilog extends App {
  Config.spinal.generateVerilog(new bitmap_addr())
}