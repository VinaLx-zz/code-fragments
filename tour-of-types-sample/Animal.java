class Animal {
    public String sound() { return "Awww!"; }
}
class Dog extends Animal {
    public String sound() { return "bark!!"; }
}
class Cat extends Animal {
    public String sound() { return "Meow"; }
}
class Cow {
    public String sound() { return "Mooooooooooo"; }
}

class Test {
    static boolean Louder(Animal a, Animal b) {
        return a.sound().length() > b.sound().length();
    }
    static void Animals() {
        Dog dog = new Dog();
        Cat cat = new Cat();
        Cow cow = new Cow();
        Louder(dog, cat); // true
        // error, Cow is not a subtype of Animal
        Louder(cow, dog); 
    }
}
