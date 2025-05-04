package castledrawing
import spinal.core._
import spinal.lib._
class draw_triangle_fill(val CORDW: Int) extends Component {
  noIoPrefix()
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val start = in Bool()
    val oe = in Bool()
    val x0, y0, x1, y1, x2, y2 = in SInt (CORDW bits)
    val x, y = out SInt (CORDW bits)
    val drawing = out Bool()
    val busy = out Bool()
    val done = out Bool()
  }
  val myClockDomain = ClockDomain(  //custom clock domain
    clock = io.clk,
    reset = io.rst,
    config = ClockDomainConfig(
      resetActiveLevel = HIGH,
      resetKind = SYNC
    )
  )
  val myArea = new ClockingArea(myClockDomain) {
    val x0s, y0s, x1s, y1s, x2s, y2s = Reg(SInt(CORDW bits)) init (0) // sorted input vertices
    val x0a, y0a, x1a, y1a, xa = Reg(SInt(CORDW bits))init (S(0, CORDW bits))// line coordinates
    val x0b, y0b, x1b, y1b, xb  = Reg(SInt(CORDW bits))init (S(0, CORDW bits))
    val x0h, x1h, xh = Reg(SInt(CORDW bits)) init (0)
    val ya,yb = SInt(CORDW bits)
    val prev_y = Reg(SInt(CORDW bits)) init (0) // previous y value for edges
    val prev_xa, prev_xb = Reg(SInt(CORDW bits)) init (0)// previous x-values for horizontal line
    val x0h_next = Mux(prev_xa > prev_xb, prev_xb, prev_xa)
    val x1h_next = Mux(prev_xa > prev_xb, prev_xa, prev_xb)
    val yReg = Reg(SInt(CORDW bits)) init (0)
    io.y := yReg
    val oe_a, oe_b, oe_h = Bool() // line control signals
    val drawing_h = Bool()
    val busy_a, busy_b, busy_h = Bool()
    val b_edge = Reg(Bool()) init (False) // which B edge are we drawing?
    val busy_p1 = Reg(Bool()) init (False)
    val done_p1 = Reg(Bool()) init (False)
    val start_a_reg = RegInit(False)
    val start_b_reg = RegInit(False)
    val start_h_reg = RegInit(False)
    object State extends SpinalEnum { //state machine using SpinalEnum
      val IDLE, SORT_0, SORT_1, SORT_2, INIT_A, INIT_B0, INIT_B1, START_A, START_B, EDGE, START_H, H_LINE, DONE = newElement()
    }
    val state = Reg(State()) init (State.IDLE)
    switch(state) {
      is(State.IDLE) {
        busy_p1 := False
        done_p1 := False
        when(io.start) {
          state := State.SORT_0
        }
      }
      is(State.SORT_0) {
        when(io.y0 > io.y2) {
          x0s := io.x2
          y0s := io.y2
          x2s := io.x0
          y2s := io.y0
        } otherwise {
          x0s := io.x0
          y0s := io.y0
          x2s := io.x2
          y2s := io.y2
        }
        busy_p1:=True
        state := State.SORT_1
      }
      is(State.SORT_1) {
        when(y0s > io.y1) {
          x0s := io.x1
          y0s := io.y1
          x1s := x0s
          y1s := y0s
        } otherwise {
          x1s := io.x1
          y1s := io.y1
        }
        state := State.SORT_2
      }
      is(State.SORT_2) {
        when(y1s > y2s) {
          x1s := x2s
          y1s := y2s
          x2s := x1s
          y2s := y1s
        }
        state := State.INIT_A
      }
      is(State.INIT_A) {
        x0a := x0s
        y0a := y0s
        x1a := x2s
        y1a := y2s
        prev_xa := x0s
        prev_xb := x0s
        state := State.INIT_B0
      }
      is(State.INIT_B0) {
       // b_edge := True
        x0b := x0s
        y0b := y0s
        x1b := x1s
        y1b := y1s
        prev_y := y0s
        state := State.START_A
        start_a_reg:=True
      }
      is(State.INIT_B1) {
        b_edge := True
        x0b := x1s
        y0b := y1s
        x1b := x2s
        y1b := y2s
        prev_y := y1s
        start_b_reg := True
        state := State.START_B
      }
      is(State.START_A) {
        start_a_reg := False
        start_b_reg := True
        state := State.START_B
      }
      is(State.START_B) {
        start_b_reg := False
        state := State.EDGE
      }
      is(State.EDGE) {
        when((ya =/= prev_y || !busy_a) && (yb =/= prev_y || !busy_b)&& (ya=/=0 && yb=/=0 &&prev_y=/=0)) {
          x0h := x0h_next
          x1h := x1h_next
          start_h_reg := True
          state := State.START_H
        }
      }
      is(State.START_H) {
        start_h_reg := False
        state := State.H_LINE
      }
      is(State.H_LINE) {
        when(!busy_h) {
          prev_y := yb
          prev_xa := xa
          prev_xb := xb
          start_h_reg := False
          when(!busy_b) {
            when(busy_a && !b_edge) {
              state := State.INIT_B1
            } otherwise {
              done_p1 := True
              state := State.DONE
            }
          } otherwise {
            state := State.EDGE
          }
        }
      }
      is(State.DONE) {
        done_p1 := False
        state := State.IDLE
      }
    }
    io.x := xh
    yReg := prev_y
    io.drawing := drawing_h
    io.busy := busy_p1
    io.done := done_p1
    val drawEdgeA = new draw_line(CORDW)
    drawEdgeA.io.clk := io.clk
    drawEdgeA.io.rst := io.rst
    drawEdgeA.io.start := start_a_reg
    drawEdgeA.io.oe := oe_a
    drawEdgeA.io.x0 := x0a
    drawEdgeA.io.y0 := y0a
    drawEdgeA.io.x1 := x1a
    drawEdgeA.io.y1 := y1a
    xa :=  drawEdgeA.io.x
    ya :=  drawEdgeA.io.y
    busy_a := drawEdgeA.io.busy
    val drawEdgeB = new draw_line(CORDW)
    drawEdgeB.io.clk := io.clk
    drawEdgeB.io.rst := io.rst
    drawEdgeB.io.start := start_b_reg
    drawEdgeB.io.oe := oe_b
    drawEdgeB.io.x0 := x0b
    drawEdgeB.io.y0 := y0b
    drawEdgeB.io.x1 := x1b
    drawEdgeB.io.y1 := y1b
    xb :=  drawEdgeB.io.x
    yb :=  drawEdgeB.io.y
    busy_b := drawEdgeB.io.busy
    val drawHLine = new draw_line_1d_tri(CORDW)
    drawHLine.io.clk := io.clk
    drawHLine.io.rst := io.rst
    drawHLine.io.start := start_h_reg
    drawHLine.io.oe := oe_h
    drawHLine.io.x0 := x0h
    drawHLine.io.x1 := x1h
    xh := drawHLine.io.x
    drawing_h := drawHLine.io.drawing
    busy_h := drawHLine.io.busy
    oe_a := (state === State.EDGE && ya === prev_y)
    oe_b := (state === State.EDGE && yb === prev_y)
    oe_h := io.oe
  }
}
object draw_triangle_fillVerilog extends App {
  Config.spinal.generateVerilog(new draw_triangle_fill(16))
}