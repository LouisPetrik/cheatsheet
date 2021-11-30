# Java EE Cheatsheet

Let's have a look at yesterday's tech stack.

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
