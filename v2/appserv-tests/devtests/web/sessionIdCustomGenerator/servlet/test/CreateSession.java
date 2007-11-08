package test;

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

public class CreateSession extends HttpServlet {

    public void doGet(HttpServletRequest req, HttpServletResponse res)
            throws ServletException, IOException {

        HttpSession session = req.getSession(true);
        if (!"abc".equals(session.getId())) {
            throw new ServletException("Wrong session id");
        }
    }
}
