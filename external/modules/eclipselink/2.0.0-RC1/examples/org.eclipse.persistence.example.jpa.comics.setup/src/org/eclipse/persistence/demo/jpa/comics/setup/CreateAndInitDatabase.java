/**
 * Copyright (c) 2007 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License v1.0, which accompanies this distribution
 * and is available at http://www.eclipse.org/legal/epl-v10.html.
 *
 * @author shsmith
 */

package org.eclipse.persistence.demo.jpa.comics.setup; 

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;

import org.eclipse.persistence.example.jpa.comics.model.annotated.Issue;
import org.eclipse.persistence.example.jpa.comics.model.annotated.Publisher;
import org.eclipse.persistence.example.jpa.comics.model.annotated.Title;
import org.eclipse.persistence.jpa.osgi.PersistenceProvider;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class CreateAndInitDatabase implements BundleActivator {

	public void start(BundleContext context) throws Exception {
		EntityManagerFactory emf = null;
		EntityManager em = null;
		try {
			Map<String, Object> properties = new HashMap<String, Object>();
			properties.put("eclipselink.ddl-generation", "drop-and-create-tables");
			properties.put("eclipselink.ddl-generation.output-mode", "database");
			properties.put("eclipselink.classloader", this.getClass().getClassLoader());
			emf = new PersistenceProvider()
					.createEntityManagerFactory("comics", properties);
			em = emf.createEntityManager();
			em.getTransaction().begin();
	
			URL publisherFileURL = context.getBundle().getResource("publisher.tab");
			Map<Integer, Publisher> publishers = loadPublishers(publisherFileURL);
			persist(em, publishers.values());
	
			URL titleFileURL = context.getBundle().getResource("title.tab");
			Map<Integer, Title> titles = loadTitles(titleFileURL, publishers);
			persist(em, titles.values());
	
			URL issueFileURL = context.getBundle().getResource("issue.tab");
			Map<Integer, Issue> issues = loadIssues(issueFileURL, titles);
			persist(em, issues.values());
	
			// update DEFAULT sequence to highest used PK
			List<Integer> allIds = new ArrayList<Integer>();
			allIds.addAll(publishers.keySet());
			allIds.addAll(titles.keySet());
			allIds.addAll(issues.keySet());
			Collections.sort(allIds);
			Integer highestId = allIds.get(allIds.size() - 1);
			String sequenceSql = "UPDATE SEQUENCE SET SEQ_COUNT=" + ++highestId + " WHERE SEQ_NAME = 'SEQ_GEN'";
			em.createNativeQuery(sequenceSql).executeUpdate();
	
			em.getTransaction().commit();
		} finally {
			if (em != null) {
				em.close();
			}
			if (emf != null) {
				emf.close();
			}
		}
	}
	
	public void stop(BundleContext context) throws Exception {
	}
	
	private static void persist(EntityManager em, Collection<?> values) {
		for (Object object : values) {
			em.persist(object);
		}
	}

	private static Map<Integer, Issue> loadIssues(URL fileURL,
			Map<Integer, Title> titles) throws Exception {
		Map<Integer, Issue> issues = new HashMap<Integer, Issue>();
		BufferedReader reader = null;
		try {
			reader = new BufferedReader(new InputStreamReader(fileURL.openStream()));
			String nextLine = null;
			while ((nextLine = reader.readLine()) != null) {
				Issue issue = buildIssue(nextLine, titles);
				issues.put(issue.getId(), issue);
			}
		} finally {
			if (reader != null) {
				reader.close();
			}
		}
		return issues;
	}

	private static Issue buildIssue(String line, Map<Integer, Title> titles) {
		//TITLE,ISSUE_NUMBER,STORY_ARC,CONDITION,COMMENTS,COPIES,ID,TITLE_ID
		String[] columns = line.split("\t");
		Issue issue = new Issue();
		issue.setIssueNum(Integer.valueOf(columns[1]));
		issue.setCondition(columns[3]);
		issue.setComments(columns[4]);
		String numCopiesString = columns[5];
		if (numCopiesString.length() > 0) {
			issue.setCopies(Integer.valueOf(numCopiesString));
		}
		issue.setId(Integer.valueOf(columns[6]));
		issue.setTitle(titles.get(Integer.valueOf(columns[7])));
		return issue;
	}

	private static Map<Integer, Publisher> loadPublishers(URL fileURL )
			throws Exception {
		Map<Integer, Publisher> publishers = new HashMap<Integer, Publisher>();
		BufferedReader reader = null;
		try {
			reader = new BufferedReader(new InputStreamReader(fileURL.openStream()));
			String nextLine = null;
			while ((nextLine = reader.readLine()) != null) {
				Publisher publisher = buildPublisher(nextLine);
				publishers.put(publisher.getId(), publisher);
			}
		} finally {
			if (reader != null) {
				reader.close();
			}
		}
		return publishers;
	}

	private static Publisher buildPublisher(String line) {
		//NAME ID
		String[] columns = line.split("\t");
		assert columns.length == 2;
		Publisher publisher = new Publisher();
		publisher.setName(columns[0]);
		publisher.setId(Integer.valueOf(columns[1]));
		return publisher;
	}

	private static Map<Integer, Title> loadTitles(URL fileURL,
			Map<Integer, Publisher> publishers) throws Exception {
		Map<Integer, Title> titles = new HashMap<Integer, Title>();
		BufferedReader reader = null;
		try {
			reader = new BufferedReader(new InputStreamReader(fileURL.openStream()));
			String nextLine = null;
			while ((nextLine = reader.readLine()) != null) {
				Title title = buildTitle(nextLine, publishers);
				titles.put(title.getId(), title);
			}
		} finally {
			if (reader != null) {
				reader.close();
			}
		}
		return titles;
	}

	private static Title buildTitle(String line,
			Map<Integer, Publisher> publishers) {
		//NAME,PUBLISHER,FORMAT,SUBSCRIBED,ID,PUBLISHER_ID
		String[] columns = line.split("\t");
		Title title = new Title();
		title.setName(columns[0]);
		title.setFormat(columns[2]);
		title.setId(Integer.valueOf(columns[4]));
		title.setPublisher(publishers.get(Integer.valueOf(columns[5])));
		return title;
	}


}
