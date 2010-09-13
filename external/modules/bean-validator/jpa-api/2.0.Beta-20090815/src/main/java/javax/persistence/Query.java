// $Id: Query.java 17305 2009-08-14 18:29:45Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Interface used to control query execution.
 */
public interface Query {
	/**
	 * Execute a SELECT query and return the query results
	 * as a List.
	 *
	 * @return a list of the results
	 *
	 * @throws IllegalStateException		if called for a Java
	 *                                      Persistence query language UPDATE or DELETE statement
	 * @throws QueryTimeoutException		if the query execution exceeds
	 *                                      the query timeout value set
	 * @throws TransactionRequiredException if a lock mode has
	 *                                      been set and there is no transaction
	 * @throws PessimisticLockException	 if pessimistic locking
	 *                                      fails and the transaction is rolled back
	 * @throws LockTimeoutException		 if pessimistic locking
	 *                                      fails and only the statement is rolled back
	 */
	public List getResultList();

	/**
	 * Execute a SELECT query that returns a single result.
	 *
	 * @return the result
	 *
	 * @throws NoResultException			if there is no result
	 * @throws NonUniqueResultException	 if more than one result
	 * @throws IllegalStateException		if called for a Java
	 *                                      Persistence query language UPDATE or DELETE statement
	 * @throws QueryTimeoutException		if the query execution exceeds *the query timeout value set
	 * @throws TransactionRequiredException if a lock mode has
	 *                                      been set and there is no transaction
	 * @throws PessimisticLockException	 if pessimistic locking
	 *                                      fails and the transaction is rolled back
	 * @throws LockTimeoutException		 if pessimistic locking
	 *                                      fails and only the statement is rolled back
	 */
	public Object getSingleResult();

	/**
	 * Execute an update or delete statement.
	 *
	 * @return the number of entities updated or deleted
	 *
	 * @throws IllegalStateException		if called for a Java
	 *                                      Persistence query language SELECT statement or for
	 *                                      a criteria query
	 * @throws TransactionRequiredException if there is
	 *                                      no transaction
	 * @throws QueryTimeoutException		if the statement execution
	 *                                      exceeds the query timeout value set
	 */
	public int executeUpdate();

	/**
	 * Set the maximum number of results to retrieve.
	 *
	 * @param maxResult
	 *
	 * @return the same query instance
	 *
	 * @throws IllegalArgumentException if argument is negative
	 */
	public Query setMaxResults(int maxResult);

	/**
	 * The maximum number of results the query object was set to
	 * retrieve. Returns Integer.MAX_VALUE if setMaxResults was not
	 * applied to the query object.
	 *
	 * @return maximum number of results
	 */
	public int getMaxResults();

	/**
	 * Set the position of the first result to retrieve.
	 *
	 * @param startPosition position of the first result, numbered from 0
	 *
	 * @return the same query instance
	 *
	 * @throws IllegalArgumentException if argument is negative
	 */
	public Query setFirstResult(int startPosition);

	/**
	 * The position of the first result the query object was set to
	 * retrieve. Returns 0 if setFirstResult was not applied to the
	 * query object.
	 *
	 * @return position of first result
	 */
	public int getFirstResult();

	/**
	 * Set a query hint.
	 * If a vendor-specific hint is not recognized, it is silently
	 * ignored.
	 * Portable applications should not rely on the standard timeout
	 * hint. Depending on the database in use and the locking
	 * mechanisms used by the provider, the hint may or may not
	 * be observed.
	 * * @param hintName
	 *
	 * @param value
	 *
	 * @return the same query instance
	 *
	 * @throws IllegalArgumentException if the second argument is not
	 *                                  valid for the implementation
	 */
	public Query setHint(String hintName, Object value);

	/**
	 * Get the hints and associated values that are in effect for
	 * the query instance.
	 *
	 * @return query hints
	 */
	public Map<String, Object> getHints();

	/**
	 * Get the names of the hints that are supported for query
	 * objects. These hints correspond to hints that may be passed
	 * to the methods of the Query interface that take hints as
	 * arguments or used with the NamedQuery and NamedNativeQuery
	 * annotations. These include all standard query hints as well as
	 * vendor-specific hints supported by the provider. These hints
	 * may or may not currently be in effect.
	 *
	 * @return hints
	 */
	public Set<String> getSupportedHints();

	/**
	 * Bind an argument to a named parameter.
	 *
	 * @param name the parameter name
	 * @param value
	 *
	 * @return the same query instance
	 *
	 * @throws IllegalArgumentException if parameter name does not
	 *                                  correspond to a parameter of the query or if
	 *                                  the argument is of incorrect type
	 */
	public Query setParameter(String name, Object value);

	/**
	 * Bind an instance of java.util.Date to a named parameter.
	 *
	 * @param name
	 * @param value
	 * @param temporalType
	 *
	 * @return the same query instance
	 *
	 * @throws IllegalArgumentException if parameter name does not
	 *                                  correspond to a parameter of the query
	 */
	public Query setParameter(String name, Date value,
							  TemporalType temporalType);

	/**
	 * Bind an instance of java.util.Calendar to a named parameter.
	 *
	 * @param name
	 * @param value
	 * @param temporalType
	 *
	 * @return the same query instance
	 *
	 * @throws IllegalArgumentException if parameter name does not
	 *                                  correspond to a parameter of the query
	 */
	public Query setParameter(String name, Calendar value,
							  TemporalType temporalType);

	/**
	 * Bind an argument to a positional parameter.
	 *
	 * @param position
	 * @param value
	 *
	 * @return the same query instance
	 *
	 * @throws IllegalArgumentException if position does not
	 *                                  correspond to a positional parameter of
	 *                                  the query or if the argument is of incorrect
	 *                                  type
	 */
	public Query setParameter(int position, Object value);

	/**
	 * Bind an instance of java.util.Date to a positional parameter.
	 *
	 * @param position
	 * @param value
	 * @param temporalType
	 *
	 * @return the same query instance
	 *
	 * @throws IllegalArgumentException if position does not
	 *                                  correspond to a positional parameter of the
	 *                                  query
	 */
	public Query setParameter(int position, Date value,
							  TemporalType temporalType);

	/**
	 * Bind an instance of java.util.Calendar to a positional
	 * parameter.
	 *
	 * @param position
	 * @param value
	 * @param temporalType
	 *
	 * @return the same query instance
	 *
	 * @throws IllegalArgumentException if position does not
	 *                                  correspond to a positional parameter of the *query
	 */
	public Query setParameter(int position, Calendar value,
							  TemporalType temporalType);

	/**
     * Get the parameter objects corresponding to the declared
     * parameters of the query.
     * Returns empty set if the query has no parameters.
     * This method is not required to be supported for native
     * queries.
     * @return set of the parameter objects
     * @throws IllegalStateException if invoked on a native
     *         query when the implementation does not support
     *         this use
     */
    Set<Parameter<?>> getParameters();

    /**
     * Get the parameter object corresponding to the declared
     * parameter of the given name.
     * This method is not required to be supported for native
     * queries.
     * @param name
     * @return parameter object
     * @throws IllegalArgumentException if the parameter of the
     *         specified name does not exist
     * @throws IllegalStateException if invoked on a native
     *         query when the implementation does not support
     *         this use
     */
    Parameter<?> getParameter(String name);

    /**
     * Get the parameter object corresponding to the declared
     * positional parameter with the given position.
     * This method is not required to be supported for native
     * queries.
     * @param position
     * @return parameter object
     * @throws IllegalArgumentException if the parameter with the
     *         specified position does not exist
     * @throws IllegalStateException if invoked on a native
     *         query when the implementation does not support
     *         this use
     */
    Parameter<?> getParameter(int position);

    /**
     * Get the parameter of the given name and type.
     * This method is required to be supported for criteria queries
     * only.
     * @param name
     * @param type
     * @return parameter object
     * @throws IllegalArgumentException if the parameter of the
     *         specified name does not exist or is not assignable
     *         to the type
     * @throws IllegalStateException if invoked on a native
     *         query or Java Persistence query language query when
     *         the implementation does not support this use
     */
    <T> Parameter<T> getParameter(String name, Class<T> type);

    /**
     * Get the positional parameter with the given position and type.
     * This method is required to be supported for criteria queries
     * only.
     * @param position
     * @param type
     * @return parameter object
     * @throws IllegalArgumentException if the parameter with the
     *         specified position does not exist or is not assignable
     *         to the type
     * @throws IllegalStateException if invoked on a native
     *         query or Java Persistence query language query when
     *         the implementation does not support this use
     */
    <T> Parameter<T> getParameter(int position, Class<T> type);

    /**
     * Return a boolean indicating whether a value has been bound
     * to the parameter.
     * @param param parameter object
     * @return boolean indicating whether parameter has been bound
     */
    boolean isBound(Parameter<?> param);

    /**
     * Return the value bound to the parameter.
     * @param param parameter object
     * @return parameter value
     * @throws IllegalStateException if the parameter has not been
     *         been bound
     */
    <T> T getParameterValue(Parameter<T> param);

    /**
     * Return the value bound to the named parameter.
     * @param name
     * @return parameter value
     * @throws IllegalStateException if the parameter has not been
     *         been bound
     * @throws IllegalArgumentException if the parameter of the
     *         specified name does not exist
     */
    Object getParameterValue(String name);

    /**
     * Return the value bound to the positional parameter.
     * @param position
     * @return parameter value
     * @throws IllegalStateException if the parameter has not been
     *         been bound
     * @throws IllegalArgumentException if the parameter with the
     *         specified position does not exist
     */
    Object getParameterValue(int position);

	/**
	 * Set the flush mode type to be used for the query execution.
	 * The flush mode type applies to the query regardless of the
	 * flush mode type in use for the entity manager.
	 *
	 * @param flushMode
	 */
	public Query setFlushMode(FlushModeType flushMode);

	/**
	 * The flush mode in effect for the query execution. If a flush
	 * mode has not been set for the query object, returns the flush
	 * mode in effect for the entity manager.
	 *
	 * @return flush mode
	 */
	public FlushModeType getFlushMode();

	/**
	 * Set the lock mode type to be used for the query execution.
	 *
	 * @param lockMode
	 *
	 * @throws IllegalStateException if the query is found not to be *a Java Persistence query language SELECT query
	 *                               or a Criteria API query
	 */
	public Query setLockMode(LockModeType lockMode);

	/**
	 * Get the current lock mode for the query.
	 *
	 * @return lock mode
	 *
	 * @throws IllegalStateException if the query is found not to be *a Java Persistence query language SELECT query
	 *                               or a Criteria API query
	 */
	public LockModeType getLockMode();

	/**
	 * Return an object of the specified type to allow access to the
	 * provider-specific API. If the provider's Query implementation
	 * does not support the specified class, the PersistenceException
	 * is thrown.
	 *
	 * @param cls the class of the object to be returned. This is
	 * normally either the underlying Query implementation class
	 * or an interface that it implements.
	 *
	 * @return an instance of the specified class
	 *
	 * @throws PersistenceException if the provider does not support
	 *                              the call.
	 */
	public <T> T unwrap(Class<T> cls);
}
