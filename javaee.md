# Java EE Cheatsheet

Let's have a look at yesterday's tech stack.

## Forms-Stuff

```html
<form method="POST" action="ServletName"></form>
```

in java:

```java
@WebServlet("/ServletName")
```

## GET and POST

### Getting a POST requests

```java
protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
   String loginname = request.getParameter("loginname");
   String passwort = request.getParameter("passwort");

   // redirecting
   request.getRequestDispatcher("index.html").forward(request, response);
}
```

### Answering a GET request

```java
import java.io.PrintWriter;

protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	response.setContentType("text/html");
	PrintWriter out = response.getWriter();

	out.println("<h1>Hello world</h1>");
}
```

## Sessions

The following code uses a session to track number of reloads.

```java
import javax.servlet.http.HttpSession;

// inside the doGet request handler method:
HttpSession session = request.getSession();

//
Integer count = (Integer) session.getAttribute("tracker.count");

if (count == null) {
   count = new Integer(1);
} else {
   count = new Integer(count.intValue() + 1);
}

session.setAttribute("tracker.count", count);
out.println("This page was opened: " + count + " times");

```

### Doing a minimal auth with sessions:

```java
// save the input from a form
String password = request.getParameter("password");

if (password.equals("xxxx")) {
   request.getRequestDispatcher("output.jsp").forward(request, response);
}
```

## JSP

### Using the templating language for parameters:

```html
<body>
	Hello ${ loginname }!
</body>
```

The wrapping whitespaces are necessary.

### Including other pages

```html
<jsp:include page="navbar.jsp" />
```
