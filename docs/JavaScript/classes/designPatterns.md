### question
Can you give examples of JavaScript classes that define some of the Gang of Four Design Patterns ?

### answer
### Examples of JavaScript Classes Implementing Gang of Four Design Patterns

#### **1. Singleton Pattern**
Ensures a class has only one instance and provides a global point of access to it.

```javascript
class Singleton {
  constructor() {
    if (!Singleton.instance) {
      Singleton.instance = this;
    }
    return Singleton.instance;
  }

  someMethod() {
    console.log("Singleton method");
  }
}

const instance1 = new Singleton();
const instance2 = new Singleton();
console.log(instance1 === instance2); // true
```

#### **2. Factory Pattern**
Defines an interface for creating an object, but lets subclasses alter the type of objects that will be created.

```javascript
class Car {
  constructor(model) {
    this.model = model;
  }
}

class CarFactory {
  createCar(model) {
    return new Car(model);
  }
}

const factory = new CarFactory();
const car1 = factory.createCar("Sedan");
const car2 = factory.createCar("SUV");
console.log(car1.model); // Sedan
console.log(car2.model); // SUV
```

#### **3. Observer Pattern**
Defines a one-to-many dependency between objects so that when one object changes state, all its dependents are notified and updated automatically.

```javascript
class Subject {
  constructor() {
    this.observers = [];
  }

  addObserver(observer) {
    this.observers.push(observer);
  }

  notifyObservers(message) {
    this.observers.forEach(observer => observer.update(message));
  }
}

class Observer {
  update(message) {
    console.log(`Observer received: ${message}`);
  }
}

const subject = new Subject();
const observer1 = new Observer();
const observer2 = new Observer();

subject.addObserver(observer1);
subject.addObserver(observer2);

subject.notifyObservers("Hello Observers!"); // Both observers receive the message
```

#### **4. Strategy Pattern**
Defines a family of algorithms, encapsulates each one, and makes them interchangeable. Strategy lets the algorithm vary independently from clients that use it.

```javascript
class Strategy {
  execute(a, b) {
    throw new Error("This method should be overridden!");
  }
}

class AddStrategy extends Strategy {
  execute(a, b) {
    return a + b;
  }
}

class SubtractStrategy extends Strategy {
  execute(a, b) {
    return a - b;
  }
}

class Context {
  setStrategy(strategy) {
    this.strategy = strategy;
  }

  executeStrategy(a, b) {
    return this.strategy.execute(a, b);
  }
}

const context = new Context();
context.setStrategy(new AddStrategy());
console.log(context.executeStrategy(5, 3)); // 8

context.setStrategy(new SubtractStrategy());
console.log(context.executeStrategy(5, 3)); // 2
```

These examples illustrate how JavaScript classes can be used to implement some of the Gang of Four design patterns¹²³.

¹: [GitHub - Design Patterns JS](https://github.com/fbeline/Design-Patterns-JS)
²: [Dofactory - JavaScript Design Patterns](https://www.dofactory.com/javascript/design-patterns/)
³: [freeCodeCamp - JavaScript Design Patterns](https://www.freecodecamp.org/news/javascript-design-patterns-explained/)

Source: Conversation with Copilot, 9/21/2024
(1) All the 23 (GoF) design patterns implemented in Javascript. https://github.com/fbeline/Design-Patterns-JS.
(2) gang-of-four-design-patterns · GitHub Topics · GitHub. https://github.com/topics/gang-of-four-design-patterns?l=javascript.
(3) JavaScript Design Patterns - Dofactory. https://www.dofactory.com/javascript/design-patterns/.
(4) JavaScript Design Patterns – Explained with Examples - freeCodeCamp.org. https://www.freecodecamp.org/news/javascript-design-patterns-explained/.
(5) Gang of Four (GoF) Design Patterns in JavaScript - GitHub. https://github.com/jake-knerr/gof-design-patterns-js.
(6) goodreads.com. https://www.goodreads.com/book/show/85009.Design_Patterns.