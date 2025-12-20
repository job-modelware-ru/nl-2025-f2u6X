use std::f32::consts::PI;

// =======================================
// 1. Инкапсуляция (encapsulation)
// =======================================
mod encapsulation {
    pub struct Inner {
        private: i32,
        pub public: i32,
    }

    impl Inner {
        pub fn new(private: i32, public: i32) -> Self {
            Self { private, public }
        }

        pub fn show_private(&self) {
            println!("private = {}", self.private);
        }
    }
}

fn example_encapsulation() {
    let inner = encapsulation::Inner::new(10, 20);

    // Доступно
    println!("public = {}", inner.public);
    inner.show_private();

    // ❌ Ошибка компиляции (как в слайдах)
    // println!("{}", inner.private);
}

// =======================================
// 2. Наследование → трейты + композиция
// =======================================
trait Shape {
    fn area(&self) -> f32;
}

trait HasAngles: Shape {
    fn angles_count(&self) -> u32;
}

struct Rectangle {
    x: f32,
    y: f32,
}

impl Shape for Rectangle {
    fn area(&self) -> f32 {
        self.x * self.y
    }
}

impl HasAngles for Rectangle {
    fn angles_count(&self) -> u32 {
        4
    }
}

struct Circle {
    r: f32,
}

impl Shape for Circle {
    fn area(&self) -> f32 {
        self.r.powi(2) * PI
    }
}

fn example_inheritance() {
    let rect = Rectangle { x: 3.0, y: 4.0 };
    let circle = Circle { r: 5.0 };

    println!("Rectangle area = {}", rect.area());
    println!("Rectangle angles = {}", rect.angles_count());
    println!("Circle area = {}", circle.area());
}

// =======================================
// 3. Динамический полиморфизм (dyn Trait)
// =======================================
trait Draw {
    fn draw(&self);
}

struct Button {
    label: String,
}

impl Draw for Button {
    fn draw(&self) {
        println!("Draw Button: {}", self.label);
    }
}

struct TextField {
    value: String,
}

impl Draw for TextField {
    fn draw(&self) {
        println!("Draw TextField: {}", self.value);
    }
}

struct Screen {
    components: Vec<Box<dyn Draw>>,
}

impl Screen {
    fn run(&self) {
        for component in self.components.iter() {
            component.draw();
        }
    }
}

fn example_dynamic_polymorphism() {
    let screen = Screen {
        components: vec![
            Box::new(Button {
                label: "OK".to_string(),
            }),
            Box::new(TextField {
                value: "Hello".to_string(),
            }),
        ],
    };

    screen.run();
}

// =======================================
// 4. Статический полиморфизм (generics)
// =======================================
fn draw_shape<T: Draw>(shape: &T) {
    shape.draw();
}

#[derive(Clone)]
struct Square {
    side: f32,
}

impl Draw for Square {
    fn draw(&self) {
        println!("Draw Square with side {}", self.side);
    }
}

fn process_shapes<T, U>(shape1: &T, shape2: &U)
where
    T: Draw + Clone,
    U: Draw + PartialEq,
{
    shape1.draw();
    shape2.draw();
}

fn example_static_polymorphism() {
    let button = Button {
        label: "Submit".to_string(),
    };
    let square = Square { side: 10.0 };

    draw_shape(&button);
    draw_shape(&square);

    let another_button = Button {
        label: "Submit".to_string(),
    };

    process_shapes(&button, &another_button);
}

// =======================================
// main
// =======================================
fn main() {
    println!("--- ENCAPSULATION ---");
    example_encapsulation();

    println!("\n--- INHERITANCE VIA TRAITS ---");
    example_inheritance();

    println!("\n--- DYNAMIC POLYMORPHISM ---");
    example_dynamic_polymorphism();

    println!("\n--- STATIC POLYMORPHISM ---");
    example_static_polymorphism();
}
