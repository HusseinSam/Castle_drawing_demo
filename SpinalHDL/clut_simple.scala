package castledrawing
import spinal.core._
case class clut_simple(COLRW: Int = 12,  // colour output width (bits)
                       CIDXW: Int = 4,   // colour index width (bits)
                       F_PAL: String = "" // init file for colour palette
                     ) extends Component {
  noIoPrefix()
  val io = new Bundle {
    val clk_write  = in Bool()                              // write clock
    val clk_read   = in Bool()                              // read clock
    val we         = in Bool()                              // write enable
    val cidx_write = in UInt(CIDXW bits)                     // colour index to write
    val cidx_read  = in UInt(CIDXW bits)                     // colour index to read
    val colr_in    = in Bits(COLRW bits)                     // write colour
    val colr_out   = out Bits(COLRW bits)                    // read colour
  }
  // Instantiate the BRAM with the specified parameters// 2^CIDXW
  val bram_clut = bram_sdp( WIDTH = COLRW, DEPTH = 1 << CIDXW, INIT_F = F_PAL
  )
  // Connect the input and output ports
  bram_clut.io.clk_write  := io.clk_write
  bram_clut.io.clk_read   := io.clk_read
  bram_clut.io.we         := io.we
  bram_clut.io.addr_write := io.cidx_write
  bram_clut.io.addr_read  := io.cidx_read
  bram_clut.io.data_in    := io.colr_in
  io.colr_out            := bram_clut.io.data_out
}
object clut_simple_Verilog extends App {Config.spinal.generateVerilog(new clut_simple())}