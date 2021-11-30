# Java EE Cheatsheet

Let's have a look at yesterday's tech stack.

## GET and POST

### Getting a POST request:

```java
protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
   String loginname = request.getParameter("loginname");
   String passwort = request.getParameter("passwort");

   // redirecting
   request.getRequestDispatcher("index.html").forward(request, response);
}
```

### Answering a GET request:

```java
import java.io.PrintWriter;

protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	response.setContentType("text/html");
	PrintWriter out = response.getWriter();

	out.println("<h1>Hello world</h1>");
}
```
