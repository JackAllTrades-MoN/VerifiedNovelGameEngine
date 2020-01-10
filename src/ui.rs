use crate::glium;
use crate::glium::{Surface};
use crate::image;

//use std::io::Cursor;

use crate::verror::{OrError};
use crate::vconfig::{Config};
use crate::vngl::Vngl;
use crate::vngl;

type DrawnObject =
    (glium::VertexBuffer<Vertex>, glium::IndexBuffer<u16>, glium::texture::Texture2d);

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

pub fn load_tag<F: glium::backend::Facade>(display: &F,
                                           cfg: &Config,
                                           tag: &vngl::InBody) -> Option<(glium::VertexBuffer<Vertex>,
                                                                         glium::IndexBuffer<u16>,
                                                                         glium::texture::Texture2d)> {
    let project_root = cfg.project_root.to_string();
    match tag {
        vngl::InBody::Img(tagimg) =>
        {
            let x = tagimg.x.unwrap_or(0);
            let y = tagimg.y.unwrap_or(0);
            let w = tagimg.w;
            let h = tagimg.h;
            let src = project_root + "/" + &tagimg.src;
            Some(mk_2d_image(display, cfg, &src, x, y, w, h))
        },
        vngl::InBody::Selection(tagsel) =>
            None
    }
}

pub fn mk_2d_image<F: glium::backend::Facade>(
    display: &F, cfg: &Config, filename: &str,
    x:u32, y:u32, w:Option<u32>, h:Option<u32>) -> DrawnObject {
    let to_rel_w = |x: u32| -> f64 { f64::from(x*2)/cfg.window_w };
    let to_rel_h = |y: u32| -> f64 { f64::from(y*2)/cfg.window_h };
    let image = image::open(filename).unwrap().to_rgba();
    let image_dim = image.dimensions();// u32 * u32
    let image = glium::texture::RawImage2d::from_raw_rgba_reversed(
        &image.into_raw(), image_dim);
    let texture = glium::texture::Texture2d::new(display, image).unwrap();

    let relx = (f64::from(x*2)/cfg.window_w - f64::from(1.0)) as f32;
    let rely = (-f64::from(y*2)/cfg.window_h + f64::from(1.0)) as f32;
    let relw = w.map(|w| to_rel_w(w)).unwrap_or(to_rel_w(image_dim.0)) as f32;
    let relh = h.map(|h| to_rel_h(h)).unwrap_or(to_rel_h(image_dim.1)) as f32;

    let v0 = Vertex { position: [relx, rely], tex_coords: [0.0, 1.0] };
    let v1 = Vertex { position: [relx + relw, rely], tex_coords: [1.0, 1.0] };
    let v2 = Vertex { position: [relx, rely - relh], tex_coords: [0.0,0.0] };
    let v3 = Vertex { position: [relx + relw, rely - relh], tex_coords: [1.0, 0.0] };
    let vs = vec![v0, v1, v2, v3];

    let vertex_buffer = glium::VertexBuffer::new(display, &vs).unwrap();
    let indices = glium::index::IndexBuffer::new(
        display,
        glium::index::PrimitiveType::TrianglesList,
        &[0 as u16,1,2,2,3,1]).unwrap();
    (vertex_buffer, indices, texture)
}

/*
pub fn mk_2d_image_xywh<F: glium::backend::Facade>(
    display: &F, cfg: &Config, filename: &str, x: u32, y: u32, w: u32, h: u32) -> (
    glium::VertexBuffer<Vertex>, glium::IndexBuffer<u16>, glium::texture::Texture2d) {
    let image = image::open(filename).unwrap().to_rgba();
    let image_dim = image.dimensions();
    let image = glium::texture::RawImage2d::from_raw_rgba_reversed(
        &image.into_raw(), image_dim);
    let texture = glium::texture::Texture2d::new(display, image).unwrap();

    let relx = (f64::from(x*2)/cfg.window_w - f64::from(1.0)) as f32;
    let rely = (f64::from(y*2)/cfg.window_h - f64::from(1.0)) as f32;
    let relw = (f64::from(w*2)/cfg.window_w) as f32;
    let relh = (f64::from(h*2)/cfg.window_h) as f32;
    let v0 = Vertex { position: [relx, rely + relh], tex_coords: [0.0, 1.0] };
    let v1 = Vertex { position: [relx + relw, rely + relh], tex_coords: [1.0, 1.0] };
    let v2 = Vertex { position: [relx, rely], tex_coords: [0.0,0.0] };
    let v3 = Vertex { position: [relx + relw, rely], tex_coords: [1.0, 0.0] };
    let vs = vec![v0, v1, v2, v3];
    println!("vec: {:?}, {:?}, {:?}, {:?}", v0, v1, v2, v3);
    let vertex_buffer = glium::VertexBuffer::new(display, &vs).unwrap();
    let indices = glium::index::IndexBuffer::new(
        display,
        glium::index::PrimitiveType::TrianglesList,
        &[0 as u16,1,2,2,3,1]).unwrap();
    (vertex_buffer, indices, texture)
} */

pub fn from_file(cfg: &Config, layout_file: &str) -> OrError<()> {
    let dim = glium::glutin::dpi::LogicalSize::new(cfg.window_w, cfg.window_h);
    let vngl = Vngl::from_file(layout_file)?;

    let mut events_loop = glium::glutin::EventsLoop::new();
    let wb = glium::glutin::WindowBuilder::new()
        .with_dimensions(dim)
        .with_title("UI Builder");
    let cb = glium::glutin::ContextBuilder::new();
    let display = glium::Display::new(wb, cb, &events_loop).unwrap();
    let program = glium::Program::from_source(&display,
                                              VERTEX_SHADER_SRC,
                                              FRAGMENT_SHADER_SRC, None).unwrap();

    let components = vngl.body
        .iter()
        .filter_map(|inbody| load_tag(&display, cfg, inbody))
        .collect::<Vec<_>>();

/*    let (vertex_buffer, indices, texture) =
        mk_2d_image(&display, window_w, window_h,
                   "example/img/dummytitle.png", 0, 0,
                    u32::from(cfg_file.window_w),
                    u32::from(cfg_file.window_h)); */

    let mut closed = false;
    while !closed {
        let mut target = display.draw();
        target.clear_color(0.0, 0.0, 0.0, 1.0);

/*        let uniforms = uniform! {
            matrix: [
                [1.0, 0.0, 0.0, 0.0],
                [0.0, 1.0, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0f32],
            ],
            tex: &texture,
        };*/
        let params = glium::DrawParameters {
            blend: glium::draw_parameters::Blend::alpha_blending(), .. Default::default() };
        for (vbuff, idx, tex) in &components {
            let uniforms = uniform! {
                matrix: [
                    [1.0, 0.0, 0.0, 0.0],
                    [0.0, 1.0, 0.0, 0.0],
                    [0.0, 0.0, 1.0, 0.0],
                    [0.0, 0.0, 0.0, 1.0f32],
                ],
                tex: tex,
            };
//            target.draw(vbuff, idx, &program, &uniforms, &Default::default()).unwrap()
            target.draw(vbuff, idx, &program, &uniforms, &params).unwrap();
        }

//        target.draw(&vertex_buffer, &indices, &program, &uniforms,
//                    &Default::default()).unwrap();
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
