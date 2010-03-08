package example.where.service;

import java.util.Calendar;
import java.util.List;

import example.where.model.Location;
import example.where.model.geonames.AdminDivision;
import example.where.model.geonames.Point;

public interface LocationService {

    List<Point> getPoints(String cc, String admin1Code);

    List<Point> getPoints(String namePattern, String cc, String admin1Code);

    AdminDivision getAdminDivision(String cc, String adminCode);

    Location addLocation(String userId, double latitude, double longitude, String comment, Calendar start, Calendar end);
}
