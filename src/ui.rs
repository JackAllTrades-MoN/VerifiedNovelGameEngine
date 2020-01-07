use crate::glium;
use crate::glium::{Surface};
use crate::image;

//use std::io::Cursor;

use crate::verror::{OrError};
use crate::vconfig::{Config};
use crate::vngl::Vngl;

#[derive(Copy, Clone, Debug)]
pub struct Vertex {
    position: [f32; 2],
    tex_coords: [f32; 2],
//    color: [f32; 3],
}

glium::implement_vertex!(Vertex, position, tex_coords);

static VERTEX_SHADER_SRC: &str = r#"
    #version 140
    in vec2 position;
    in vec2 tex_coords;
    out vec2 v_tex_coords;

    uniform mat4 matrix;

    void main() {
        v_tex_coords = tex_coords;
        gl_Position = matrix * vec4(position, 0.0, 1.0);
    }
"#;

static FRAGMENT_SHADER_SRC: &str = r#"
    #version 140
    in vec2 v_tex_coords;
    out vec4 color;

    uniform sampler2D tex;

    void main() {
        color = texture(tex, v_tex_coords);
    }
"#;
/*
pub fn load_tag(display: glium::Display, tag: vngl::InBody) -> Option<(u32, u32)> {
    match tag {
        vngl::Img(tagimg) => None,
        vngl::Selection(tagsel) => None
    }
}*/

pub fn mk_2d_image<F: glium::backend::Facade>(display: &F,
                   window_w: f64,
                   window_h: f64,
                   filename: &str,
                   x: u32,
                   y: u32,
                   w: u32,
                   h: u32) -> (glium::VertexBuffer<Vertex>,
                               glium::IndexBuffer<u16>,
                               glium::texture::Texture2d) {
    let image = image::open(filename).unwrap().to_rgba();
    let image_dim = image.dimensions();
    let image = glium::texture::RawImage2d::from_raw_rgba_reversed(
        &image.into_raw(), image_dim);
    let texture = glium::texture::Texture2d::new(display, image).unwrap();

    let relx = (f64::from(x*2)/f64::from(window_w) - f64::from(1.0)) as f32;
    let rely = (f64::from(y*2)/f64::from(window_h) - f64::from(1.0)) as f32;
    let relw = (f64::from(w*2)/f64::from(window_w)) as f32;
    let relh = (f64::from(h*2)/f64::from(window_h)) as f32;
    let v0 = Vertex { position: [relx, rely + relh], tex_coords: [0.0, 1.0] };
    let v1 = Vertex { position: [relx + relw, rely + relh], tex_coords: [1.0, 1.0] };
    let v2 = Vertex { position: [relx, rely], tex_coords: [0.0,0.0]};
    let v3 = Vertex { position: [relx + relw, rely], tex_coords: [1.0, 0.0]};
    let vs = vec![v0, v1, v2, v3];
    println!("vec: {:?}, {:?}, {:?}, {:?}", v0, v1, v2, v3);
    let vertex_buffer = glium::VertexBuffer::new(display, &vs).unwrap();
    let indices = glium::index::IndexBuffer::new(
        display,
        glium::index::PrimitiveType::TrianglesList,
        &[0 as u16,1,2,2,3,1]).unwrap();
    (vertex_buffer, indices, texture)
}

pub fn from_file(cfg_file: &Config, layout_file: &str) -> OrError<()> {
    let window_w = f64::from(cfg_file.window_w);
    let window_h = f64::from(cfg_file.window_h);

    let vngl = Vngl::from_file(layout_file);

    let mut events_loop = glium::glutin::EventsLoop::new();
    let wb = glium::glutin::WindowBuilder::new()
        .with_dimensions(glium::glutin::dpi::LogicalSize::new(window_w, window_h))
        .with_title("UI Builder");
    let cb = glium::glutin::ContextBuilder::new();
    let display = glium::Display::new(wb, cb, &events_loop).unwrap();

    let program = glium::Program::from_source(&display,
                                              VERTEX_SHADER_SRC,
                                              FRAGMENT_SHADER_SRC, None).unwrap();

//    let vertex1 = Vertex { position: [-1.0,  1.0], tex_coords: [0.0, 1.0] };
//    let vertex2 = Vertex { position: [ 1.0,  1.0], tex_coords: [1.0, 1.0] };
//    let vertex3 = Vertex { position: [-1.0, -1.0], tex_coords: [0.0, 0.0] };
//    let vertex4 = Vertex { position: [ 1.0, -1.0], tex_coords: [1.0, 0.0] };
//    let shape = vec![vertex1, vertex2, vertex3, vertex4];
//    let shape2 = vec![vertex2, vertex3, vertex4];
//    let vertex_buffer = glium::VertexBuffer::new(&display, &shape).unwrap();
//    let vertex_buffer2 = glium::VertexBuffer::new(&display, &shape2).unwrap();
    let (vertex_buffer, indices, texture) =
        mk_2d_image(&display, window_w, window_h,
                   "example/img/dummytitle.png", 0, 0,
                    u32::from(cfg_file.window_w),
                    u32::from(cfg_file.window_h));
//    let indices = glium::index::IndexBuffer::new(
//        &display,
//        glium::index::PrimitiveType::TrianglesList,
//        &[0 as u16,1,2,2,3,1]).unwrap();
//    let indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);

//    let image = image::open("example/img/dummytitle.png").unwrap().to_rgba();
//    let image_dim = image.dimensions();
//    let image = glium::texture::RawImage2d::from_raw_rgba_reversed(&image.into_raw(), image_dim);
//    let texture = glium::texture::Texture2d::new(&display, image).unwrap();

//    let vbuff, indices, 

    let mut closed = false;
    while !closed {
        let mut target = display.draw();
        target.clear_color(0.0, 0.0, 0.0, 1.0);

        let uniforms = uniform! {
            matrix: [
                [1.0, 0.0, 0.0, 0.0],
                [0.0, 1.0, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0f32],
            ],
            tex: &texture,
        };

        target.draw(&vertex_buffer, &indices, &program, &uniforms,
                    &Default::default()).unwrap();
//        target.draw(&vertex_buffer2, &indices, &program, &uniforms,
//                    &Default::default()).unwrap();
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
