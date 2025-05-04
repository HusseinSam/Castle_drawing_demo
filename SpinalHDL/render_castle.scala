package castledrawing
import spinal.core._
import spinal.lib._

class render_castle(CORDW: Int = 16,  // signed coordinate width (bits)
                    CIDXW: Int = 4, // colour index width (bits)
                    SCALE: Int = 1 // drawing scale: 1=320x180, 2=640x360, 4=1280x720
                   ) extends Component {
  noIoPrefix()
  val io = new Bundle {
    val clk = in Bool() // clock
    val rst = in Bool() // reset
    val oe = in Bool() // output enable
    val start = in Bool() // start drawing
    val x, y = out SInt (CORDW bits) // horizontal draw position, vertical draw position
    val cidx = out UInt (CIDXW bits) // pixel colour
    val drawing = out Bool() // actively drawing
    val done = out Bool() // drawing is complete (high for one tick)
  }
  val myClockDomain = ClockDomain( //custom clock domain
    clock = io.clk,
    reset = io.rst,
    config = ClockDomainConfig(
      resetActiveLevel = HIGH,
      resetKind = SYNC
    )
  )
  val myArea = new ClockingArea(myClockDomain) {
    val RBOW_CNT = 8 // number of shapes in rainbow
    val SHAPE_CNT = 19 // number of shapes in castle
    val rbow_id = Reg(UInt(log2Up(RBOW_CNT) bits)) init (0) // rainbow shape identifier
    val shape_id = Reg(UInt(log2Up(SHAPE_CNT) bits)) init (0) // castle shape identifier
    val vx0, vy0, vx1, vy1, vx2, vy2 = Reg(SInt(CORDW bits)) // shape coords
    val vr0 = Reg(SInt(CORDW bits)) // circle radius
    val x_tri, y_tri = Reg(SInt(CORDW bits)) // triangle framebuffer coords
    val x_rect, y_rect = Reg(SInt(CORDW bits)) // rectangle framebuffer coords
    val x_circle, y_circle = Reg(SInt(CORDW bits)) // circle framebuffer coords
    val draw_done = Reg(Bool()) init (False) // combined done signal
    val draw_start_tri, drawing_tri, draw_done_tri = Reg(Bool()) init (False) // drawing triangle
    val draw_start_rect, drawing_rect, draw_done_rect = Reg(Bool()) init (False) // drawing rectangle
    val draw_start_circle, drawing_circle, draw_done_circle = Reg(Bool()) init (False) // drawing circle
    io.cidx := U(0)
    object State extends SpinalEnum {
      val IDLE, INIT_RBOW, DRAW_RBOW, INIT_CASTLE, DRAW_CASTLE, DONE = newElement()
    }
    val state = Reg(State()) init (State.IDLE)
    when(io.rst) {
      state := State.IDLE
    } otherwise {
      switch(state) {
        is(State.IDLE) {
          when(io.start) {
            state := State.INIT_RBOW
          }
        }
        is(State.INIT_RBOW) {
          draw_start_circle := True
          vx0 := 368
          vy0 := 160
          vr0 := (180 - (5 * rbow_id).asSInt).resize(16)
          io.cidx := (Mux(rbow_id === (RBOW_CNT - 1), U(0), U(2) + rbow_id)).resize(4)
          state := State.DRAW_RBOW
        }
        is(State.DRAW_RBOW) {
          draw_start_circle := False
          when(draw_done) {
            when(rbow_id === (RBOW_CNT - 1)) {
              state := State.INIT_CASTLE
            } otherwise {
              rbow_id := rbow_id + 1
              state := State.INIT_RBOW
            }
          }
        }
        is(State.INIT_CASTLE) {
          switch(shape_id) {
            is(0) {
              draw_start_rect := True;
              vx0 :=  60; vy0 :=  75;
              vx1 := 190; vy1 := 130;
              io.cidx := 0xE }
            is(1) {
              draw_start_rect := True;
              vx0 := 110; vy0 := 110;
              vx1 := 140; vy1 := 130;
              io.cidx := 0xF }
            is(2) {
              draw_start_circle := True;
              vx0 := 125; vy0 := 110; vr0 :=  15;
              io.cidx := 0xF }
            is(3) {
              draw_start_rect := True;
              vx0 :=  40; vy0 :=  50; vx1 :=  60; vy1 := 130;
              io.cidx := 0xE }
            is(4) {
              draw_start_rect := True;
              vx0 := 110; vy0 :=  45;
              vx1 := 140; vy1 :=  75;
              io.cidx := 0xE }
            is(5) {
              draw_start_rect := True;
              vx0 := 190; vy0 :=  50;
              vx1 := 210; vy1 := 130;
              io.cidx := 0xE }
            is(6) {
              draw_start_tri := True;
              vx0 :=  50; vy0 :=  35;
              vx1 :=  65; vy1 :=  50;
              vx2 :=  35; vy2 :=  50;
              io.cidx := 0x2 }
            is(7) {
              draw_start_tri := True;
              vx0 := 125; vy0 :=  25;
              vx1 := 145; vy1 :=  45;
              vx2 := 105; vy2 :=  45;
              io.cidx := 0x2 }
            is(8) {
              draw_start_tri := True;
              vx0 := 200; vy0 :=  35;
              vx1 := 215; vy1 :=  50;
              vx2 := 185; vy2 :=  50;
              io.cidx := 0x2 }
            is(9) {
              draw_start_rect := True;
              vx0 :=  46; vy0 :=  55;
              vx1 :=  54; vy1 :=  70;
              io.cidx := 0xF }
            is(10) {
              draw_start_rect := True;
              vx0 := 120; vy0 :=  50;
              vx1 := 130; vy1 :=  70;
              io.cidx := 0xF }
            is(11) {
              draw_start_rect := True;
              vx0 := 196; vy0 :=  55;
              vx1 := 204; vy1 :=  70;
              io.cidx := 0xF }
            is(12) {
              draw_start_rect := True;
              vx0 :=  63; vy0 :=  67;
              vx1 :=  72; vy1 :=  75;
              io.cidx := 0xE }
            is(13) {
              draw_start_rect := True;
              vx0 :=  80; vy0 :=  67;
              vx1 :=  89; vy1 :=  75;
              io.cidx := 0xE }
            is(14) {
              draw_start_rect := True;
              vx0 :=  97; vy0 :=  67;
              vx1 := 106; vy1 :=  75;
              io.cidx := 0xE }
            is(15) {
              draw_start_rect := True;
              vx0 := 144; vy0 :=  67;
              vx1 := 153; vy1 :=  75;
              io.cidx := 0xE }
            is(16) {
              draw_start_rect := True;
              vx0 := 161; vy0 :=  67;
              vx1 := 170; vy1 :=  75;
              io.cidx := 0xE }
            is(17) {
              draw_start_rect := True;
              vx0 := 178; vy0 :=  67;
              vx1 := 187; vy1 :=  75;
              io.cidx := 0xE }
            default {
              draw_start_rect := True;
              vx0 :=  0; vy0 := 131;
              vx1 := 319; vy1 := 179;
              io.cidx := 0x0 }
          }
          state := State.DRAW_CASTLE
        }
        is(State.DRAW_CASTLE) {
          draw_start_tri := False
          draw_start_rect := False
          draw_start_circle := False
          when(draw_done) {
            when(shape_id === (SHAPE_CNT - 1)) {
              state := State.DONE
            } otherwise {
              shape_id := shape_id + 1
              state := State.INIT_CASTLE
            }
          }
        }
        is(State.DONE) {
          state := State.DONE
        }
      }
    }
    val triangle = new draw_triangle_fill(CORDW)
    triangle.io.clk := io.clk
    triangle.io.rst := io.rst
    triangle.io.start := draw_start_tri
    triangle.io.oe := io.oe
    triangle.io.x0 := (vx0 * SCALE).resize(16)
    triangle.io.y0 := (vy0 * SCALE).resize(16)
    triangle.io.x1 := (vx1 * SCALE).resize(16)
    triangle.io.y1 := (vy1 * SCALE).resize(16)
    triangle.io.x2 := (vx2 * SCALE).resize(16)
    triangle.io.y2 := (vy2 * SCALE).resize(16)
    x_tri := triangle.io.x
    y_tri := triangle.io.y
    drawing_tri := triangle.io.drawing
    draw_done_tri := triangle.io.done
    val rectangle = new draw_rectangle_fill(CORDW)
    rectangle.io.clk := io.clk
    rectangle.io.rst := io.rst
    rectangle.io.start := draw_start_rect
    rectangle.io.oe := io.oe
    rectangle.io.x0 := (vx0 * SCALE).resize(16)
    rectangle.io.y0 := (vy0 * SCALE).resize(16)
    rectangle.io.x1 := (vx1 * SCALE).resize(16)
    rectangle.io.y1 := (vy1 * SCALE).resize(16)
    x_rect := rectangle.io.x
    y_rect := rectangle.io.y
    drawing_rect := rectangle.io.drawing
    draw_done_rect := rectangle.io.done
    val circle = new draw_circle_fill(CORDW)
    circle.io.clk := io.clk
    circle.io.rst := io.rst
    circle.io.start := draw_start_circle
    circle.io.oe := io.oe
    circle.io.x0 := (vx0 * SCALE).resize(16)
    circle.io.y0 := (vy0 * SCALE).resize(16)
    circle.io.r0 := (vr0 * SCALE).resize(16)
    x_circle := circle.io.x
    y_circle := circle.io.y
    drawing_circle := circle.io.drawing
    draw_done_circle := circle.io.done
    io.x := Mux(drawing_tri, triangle.io.x, Mux(drawing_rect, rectangle.io.x, circle.io.x))
    io.y := Mux(drawing_tri, triangle.io.y, Mux(drawing_rect, rectangle.io.y, circle.io.y))
    io.drawing := drawing_tri || drawing_rect || drawing_circle
    draw_done := draw_done_tri || draw_done_rect || draw_done_circle
    io.done := state === State.DONE
  }
}
object render_castleVerilog extends App {
    Config.spinal.generateVerilog(new render_castle())
  }