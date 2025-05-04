package castledrawing
import spinal.core._
import spinal.lib._
case class linebuffer_simple(DATAW: Int = 4, LEN: Int = 320, SCALEW: Int = 6) extends Component {
  val clk_sys   = in Bool()
  val clk_pix   = in Bool()
  val line      = in Bool()
  val line_sys  = in Bool()
  val en_in     = in Bool()
  val en_out    = in Bool()
  val scale     = in UInt(SCALEW bits)
  val data_in   = in Bits(DATAW bits)
  val data_out  = out Bits(DATAW bits)
  val cd_write = ClockDomain(clock = clk_sys, config = ClockDomainConfig(resetKind = BOOT))
  val cd_read = ClockDomain(clock = clk_pix, config = ClockDomainConfig(resetKind = BOOT))
  val bram_lb = new bram_sdp(WIDTH = DATAW, DEPTH = LEN)
  val area_write = new ClockingArea(cd_write) {// ----- Write clock domain -----
    val addr_in = Reg(UInt(log2Up(LEN) bits)) init (0); val we      = Reg(Bool()) init (False)
    when (en_in) {we := True}
    when (addr_in === (LEN - 1)) {we := False}
    when (we) {addr_in := addr_in + 1}
    when (line_sys) {we := False; addr_in := 0}
    bram_lb.io.clk_write := clk_sys
    bram_lb.io.we        := we
    bram_lb.io.addr_write := addr_in
    bram_lb.io.data_in    := data_in
  }
  val area_read = new ClockingArea(cd_read) {// ----- Read clock domain -----
    val addr_out = Reg(UInt(log2Up(LEN) bits)) init (0); val cnt_h    = Reg(UInt(SCALEW bits)) init (0)
    when (en_out) {
      when (cnt_h === (scale - 1)) { cnt_h := 0
        when (addr_out =/= (LEN - 1)) {addr_out := addr_out + 1}
      } otherwise {
        cnt_h := cnt_h + 1
      }
    }
    when (line) {
      addr_out := 0
      cnt_h := 0
    }
    bram_lb.io.clk_read := clk_pix
    bram_lb.io.addr_read := addr_out
    data_out := bram_lb.io.data_out
  }
}
object linebuffer_simple_Verilog extends App {Config.spinal.generateVerilog(new linebuffer_simple())}