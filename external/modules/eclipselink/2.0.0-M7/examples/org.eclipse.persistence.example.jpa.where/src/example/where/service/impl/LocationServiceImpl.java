package example.where.service.impl;

import java.util.Calendar;
import java.util.List;

import javax.persistence.EntityManager;

import example.where.model.Location;
import example.where.model.User;
import example.where.model.geonames.AdminDivision;
import example.where.model.geonames.Point;
import example.where.service.LocationService;

public class LocationServiceImpl extends PersistenceService implements LocationService {

    public LocationServiceImpl(ServiceFactory factory) {
        super(factory);
    }

    @Override
    public List<Point> getPoints(String cc, String admin1Code) {
        EntityManager em = getEMF().createEntityManager();

        try {
            return em.createQuery("SELECT p FROM Point p WHERE p.country.code = :CC AND p.admin1Code = :AC").setParameter("CC", cc).setParameter("AC", admin1Code).getResultList();
        } finally {
            em.close();
        }
    }

    @Override
    public List<Point> getPoints(String namePattern, String cc, String admin1Code) {
        EntityManager em = getEMF().createEntityManager();

        try {
            return em.createQuery("SELECT p FROM Point p WHERE p.name LIKE :NAME AND p.country.code = :CC AND p.admin1Code = :AC").setParameter("NAME", namePattern).setParameter("CC", cc)
                    .setParameter("AC", admin1Code).getResultList();
        } finally {
            em.close();
        }
    }

    @Override
    public AdminDivision getAdminDivision(String cc, String adminCode) {
        EntityManager em = getEMF().createEntityManager();

        try {
            return em.find(AdminDivision.class, new AdminDivision.ID(cc, adminCode));
        } finally {
            em.close();
        }
    }

    @Override
    public Location addLocation(String userId, double latitude, double longitude, String comment, Calendar start, Calendar end) {
        EntityManager em = getEMF().createEntityManager();
        em.getTransaction().begin();

        try {
            User user = em.find(User.class, userId);

            if (user == null) {
                throw new IllegalArgumentException("Unknown User Id: " + userId);
            }

            Location location = new Location(user, latitude, longitude, comment, start, end);
            em.persist(location);
            user.setCurrentLocation(location);

            return location;
        } finally {
            if (em.getTransaction().isActive()) {
                em.getTransaction().rollback();
            }
            em.close();
        }
    }
}
