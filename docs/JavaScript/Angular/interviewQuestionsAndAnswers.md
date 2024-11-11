### Angular Common Interview Questions

1. What is Angular and what are its main features?

Angular is a platform and framework for building single-page client applications using HTML and TypeScript. Main features include components, two-way data binding, dependency injection, and a modular architecture.

2. Explain the purpose of Angular modules.

Angular modules organize an application into cohesive blocks of functionality. They help with code organization, dependency management, and lazy loading.

3. How does an Angular application work?

An Angular application is a collection of components organized in modules. Components interact with each other via services and data binding, and routing manages navigation.

4. What are Angular components and how do they work?

Components are the building blocks of Angular applications. Each component has an HTML template, a TypeScript class, and optional CSS styles. Components define views and encapsulate behavior.

5. Describe the Angular CLI and its commands.

The Angular CLI is a command-line interface tool for creating, developing, and maintaining Angular applications. Common commands include ng new, ng serve, ng generate, and ng build.

6. What is two-way data binding in Angular?

Two-way data binding allows synchronization of data between the model and the view. Changes in the model automatically reflect in the view and vice versa. It is achieved using the [(ngModel)] directive.

7. Explain the difference between template-driven and reactive forms.

Template-driven forms use Angular directives in the template to create forms, while reactive forms use a more explicit, model-driven approach, providing greater control and validation logic.

8. How do you handle routing in Angular?

Routing in Angular is managed using the Angular Router module. It defines routes, path-to-component mappings, and navigation links. It enables single-page navigation without reloading the entire page.

9. What is dependency injection in Angular and why is it important?

Dependency injection is a design pattern used to manage component dependencies. It allows injecting services into components rather than creating them manually, promoting reusability and testability.

10. How do you optimize performance in an Angular application?

Techniques include lazy loading, change detection strategies, Ahead-of-Time (AOT) compilation, optimizing template expressions, and using Angular's trackBy in ngFor.

11. What are Angular directives and how are they used?

Directives are classes that add behavior to elements in Angular templates. They include component directives, attribute directives (e.g., ngClass, ngStyle), and structural directives (e.g., ngIf, ngFor).

12. Describe the use of services in Angular.

Services encapsulate reusable logic and data that can be shared across components. They are typically used for data fetching, business logic, and other operations that need to be centralized.

13. What is lazy loading and how is it implemented in Angular?

Lazy loading is a technique to load modules only when needed. It reduces the initial load time of the application. It is implemented using the loadChildren property in Angular routes.

14. How do you handle HTTP requests in Angular?

HTTP requests are managed using the HttpClient service. It provides methods like get, post, put, and delete to communicate with backend APIs.

15. What are some common Angular interview questions for freshers and experienced developers?

Common questions for freshers include basic concepts of Angular, components, and data binding. Experienced developers might be asked about performance optimization, advanced routing, and state management.