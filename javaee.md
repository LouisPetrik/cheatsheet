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

### Redirceting with JSP:

```html
<jsp:forward page="error.jsp" />
```

### Accessing the session:

```html
${ sessionScope.user }
```

## Working with Postgres

Important things to setup up:

-  A provided DatabaseConnection class in the src of the eclipse project
-  The imported driver in the WEB-INF/lib/postgres-xxx.jar

## How to: Queries etc.

Using the auto-commit flag, transaction will be executed automatically, see:

```java
autoCommit(boolean autoCommitFlag)
```

Make transaction a change: commit()
To undo changes: rollback()

It is important to close the database connection: close()

Some no-brainers:

Compared to SELECT, INSERT, UPDATE and DELETE will return the number of the affected row.

## Using prepared statements:

Prepared statements should be used, to avoid injections.

```java

PreparedStatement pstmt = con.prepareStatement(
   "SELECT * FROM person WHERE name = ?"
);

pstmt.setString(1 "Doe");
pstmt.set(1, Types.INTEGER);
pstmt.setInt(1, 5);
```

The first parameter determines the position of the questionmark in the SQL code, which will
be replaced with an actual value.

All SQL classes should consist of a try-catch block, for example:

```java
public static int getUserNumber() {
   int number = 0;

   try {
      con = DatabaseConnection.getConnection();

      PreparedStatement pstmt = con.prepareStatement(
         "SELECT COUNT (*) FROM users"
      );

      ResultSet rs = pstmt.executeQuery();

      if (rs.next()) {
         zahl = rs.getInt(1)
      }
   } catch (SQLException e) {
      System.out.println("An error occurd");
   } finally {
      try {
         con.close();
      } catch (SQLException e) {
         e.printStackTrace();
      }
   }

   return number;
}
```

Turing a whole table into an HTML table

```java

public static String resultToTable(ResultSet rs) {
   String table = "<table>";

   try {
      ResultSetMetaData rsmd = rs.getMetaData();
      int col = rsmd.getColumnCount();
      table += "<tr>";

      for (int i = 1; i <= col; i++) {
         table += "<th>" + firstUpper(rsmd.getColumnName(i)) + "</th>";
      }

      table += "</tr>";

   do {
   table += "<tr>";

   for (int i = 1; i <= col; i++) {
      table += "<td>" + rs.getString(i) + "</td>";
   }

   table += "</tr>";

   } while (rs.next());
      table += "</table> <br/>";

   } catch (SQLException e) {
      table = "Keine Ergebnisse";
   }

   return table;
}

```
