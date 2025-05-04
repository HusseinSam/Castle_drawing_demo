package castledrawing
import spinal.core._
import spinal.lib.fsm._

case class draw_line_1d(CORDW: Int) extends Component {
  noIoPrefix()
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val start = in Bool()
    val oe = in Bool()
    val x0 = in SInt(CORDW bits)
    val x1 = in SInt(CORDW bits)
    val x = out SInt(CORDW bits)
    val drawing = out Bool()
    val busy = out Bool()
    val done = out Bool()
  }
  val myClockDomain = ClockDomain(
    clock = io.clk,
    reset = io.rst,
    config = ClockDomainConfig(
      resetActiveLevel = HIGH,
      resetKind = SYNC
    )
  )
  val myArea = new ClockingArea(myClockDomain) {
    val xReg = Reg(SInt(CORDW bits))
    io.x := xReg
    val doneFlag = Reg(Bool()) init(False)
    io.done := doneFlag
    val fsm = new StateMachine {
      val idle = makeInstantEntry()
      val draw = new State()
      idle
        .whenIsActive{
          io.drawing := False
          io.busy := False
          when(doneFlag) {
            doneFlag := False
          }
          when(io.start) {
            xReg := io.x0
            io.busy := True
            goto(draw)
          }
        }
      draw
        .whenIsActive{
          io.drawing := io.oe
          io.busy := True
          when(io.oe) {
            when(xReg === io.x1) {
              doneFlag := True
              goto(idle)
            } otherwise {
              xReg := xReg + 1
            }
          }
        }
      always {
        when(io.rst) {
          goto(idle)
          io.busy := False
          io.done := False
        }
      }
    }
  }
}
object draw_line_1dVerilog extends App {Config.spinal.generateVerilog(new draw_line_1d(16))}