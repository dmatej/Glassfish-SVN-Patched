import java.io.*;
import java.util.*;
import javax.servlet.*;
import javax.servlet.http.*;

public class From extends HttpServlet {

    public void service(HttpServletRequest req, HttpServletResponse res)
        throws IOException, ServletException {

        RequestDispatcher rd = getServletContext().getRequestDispatcher("/To");
        rd.include(req, res);
    }
}
