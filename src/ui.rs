use crate::glium;
use crate::glium::Surface;
use crate::verror::{OrError};
use crate::vconfig::{Config};

pub fn from_file(cfg_file: &Config, layout_file: &str) -> OrError<()> {
    let window_w = f64::from(cfg_file.window_w);
    let window_h = f64::from(cfg_file.window_h);
    let mut events_loop = glium::glutin::EventsLoop::new();
    let wb = glium::glutin::WindowBuilder::new()
        .with_dimensions(glium::glutin::dpi::LogicalSize::new(window_w, window_h))
        .with_title("UI Builder");
    let cb = glium::glutin::ContextBuilder::new();
    let display = glium::Display::new(wb, cb, &events_loop).unwrap();

    let mut closed = false;
    while !closed {
        let mut target = display.draw();
        target.clear_color(0.0, 0.0, 0.0, 1.0);
        target.finish().unwrap();
        events_loop.poll_events(|ev| {
            match ev {
                glium::glutin::Event::WindowEvent { event, .. } =>
                    match event {
                        glium::glutin::WindowEvent::CloseRequested => closed = true,
                        _ => (),
                    },
                _ => (),
            }
        })
    }
    Ok(())
}
