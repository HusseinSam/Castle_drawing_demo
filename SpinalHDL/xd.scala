package castledrawing
import spinal.core._
import spinal.lib._
import spinal.core._
class xd extends Component {
  noIoPrefix()
  val io = new Bundle {
    val clk_src = in Bool()  // source domain clock
    val clk_dst = in Bool()  // destination domain clock
    val flag_src = in Bool() // flag in source domain
    val flag_dst = out Bool() // flag in destination domain
  }
  // toggle register in source domain
  val toggle_src = RegInit(False)
  toggle_src := toggle_src ^ io.flag_src
  // shift register in destination domain
  val shr_dst = Reg(UInt(4 bits)) init(0)
  // shift and app_dend toggle bit
  shr_dst := ((shr_dst(2 downto 0) ## toggle_src).asUInt).resize(4)
  // generate destination domain flag pulse
  io.flag_dst := shr_dst(3) ^ shr_dst(2)
}
object xd_Verilog extends App {
  Config.spinal.generateVerilog(new xd())
}