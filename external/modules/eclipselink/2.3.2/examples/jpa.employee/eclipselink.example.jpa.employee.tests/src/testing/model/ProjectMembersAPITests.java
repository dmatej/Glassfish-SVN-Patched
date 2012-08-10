/*******************************************************************************
 * Copyright (c) 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - TODO
 ******************************************************************************/
package testing.model;

import static junit.framework.Assert.*;

import java.sql.Timestamp;
import java.util.List;

import org.junit.Test;

import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.Member;
import eclipselink.example.jpa.employee.model.SmallProject;

/**
 * Test the API around the temporal version of Project and its members.
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
public class ProjectMembersAPITests {

    @Test
    public void addMemberNow() {
        Timestamp now = new Timestamp(System.currentTimeMillis());

        Employee sample = new Employee();
        SmallProject project = new SmallProject(null);

        List<Member> memberships = project.addMember(sample, now, true, now);

        assertNotNull(memberships);
        assertEquals(1, memberships.size());
        Member member = memberships.get(0);

        assertSame(sample, member.getEmployee());
        assertSame(project, member.getProject());
        assertEquals(now, member.getStart());
        assertNull(member.getEnd());

        assertEquals(1, project.getMembers().size());
        assertSame(member, project.getMember(sample));
    }

    @Test
    public void addMemberFuture() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        Timestamp future = new Timestamp(System.currentTimeMillis() + 10);

        Employee sample = new Employee();
        SmallProject project = new SmallProject(null);

        List<Member> memberships = project.addMember(sample, future, true, now);

        assertNotNull(memberships);
        assertEquals(1, memberships.size());
        Member member = memberships.get(0);

        assertSame(sample, member.getEmployee());
        assertSame(project, member.getProject());
        assertEquals(future, member.getStart());
        assertNull(member.getEnd());

        assertEquals(0, project.getMembers().size());
        assertNull(project.getMember(sample));
    }

    @Test
    public void removeMemberNow() {
        Timestamp now = new Timestamp(System.currentTimeMillis());

        Employee sample = new Employee();
        SmallProject project = new SmallProject(null);

        List<Member> memberships = project.addMember(sample, now, true, now);

        assertNotNull(memberships);
        assertEquals(1, memberships.size());
        Member member = memberships.get(0);

        assertSame(sample, member.getEmployee());
        assertSame(project, member.getProject());
        assertEquals(now, member.getStart());
        assertNull(member.getEnd());

        assertEquals(1, project.getMembers().size());
        assertSame(member, project.getMember(sample));

        project.removeAsMember(sample, now, now);

        assertEquals(0, project.getMembers().size());
    }

    @Test
    public void removeMemberFuture() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        Timestamp future = new Timestamp(System.currentTimeMillis() + 10);

        Employee sample = new Employee();
        SmallProject project = new SmallProject(null);

        List<Member> memberships = project.addMember(sample, now, true, now);

        assertNotNull(memberships);
        assertEquals(1, memberships.size());
        Member member = memberships.get(0);

        assertSame(sample, member.getEmployee());
        assertSame(project, member.getProject());
        assertEquals(now, member.getStart());
        assertNull(member.getEnd());

        assertEquals(1, project.getMembers().size());
        assertSame(member, project.getMember(sample));

        project.removeAsMember(sample, future, now);

        assertEquals(1, project.getMembers().size());
        assertSame(member, project.getMember(sample));
        assertEquals(future, member.getEnd());
    }

    @Test
    public void replaceLeadNow() {
        Timestamp past = new Timestamp(System.currentTimeMillis() - 10);
        Timestamp now = new Timestamp(System.currentTimeMillis());

        Employee sample = new Employee();
        SmallProject project = new SmallProject(null);

        List<Member> memberships = project.addMember(sample, past, true, now);

        assertNotNull(memberships);
        assertEquals(1, memberships.size());
        Member member = memberships.get(0);

        assertSame(sample, member.getEmployee());
        assertSame(project, member.getProject());
        assertEquals(past, member.getStart());
        assertNull(member.getEnd());
        assertEquals(1, project.getMembers().size());
        assertSame(member, project.getMember(sample));
        assertSame(member, project.getLead());
        assertSame(sample, project.getLead().getEmployee());

        Employee newLead = new Employee();

        memberships = project.addMember(newLead, now, true, now);

        // Verify state of project now
        assertEquals(2, project.getMembers().size());

        // Verify original lead member
        assertEquals(now, member.getEnd());

        // Verify newly created members
        assertNotNull(memberships);
        assertEquals(2, memberships.size());

        // Verify previous lead's new member
        Member newMember1 = memberships.get(0);
        assertEquals(project, newMember1.getProject());
        assertEquals(sample, newMember1.getEmployee());
        assertFalse(newMember1.isLead());
        assertNotSame(member, newMember1);
        assertSame(newMember1, project.getMember(sample));

        // Verify new lead's new member
        Member newMember2 = memberships.get(1);
        assertEquals(project, newMember2.getProject());
        assertEquals(newLead, newMember2.getEmployee());
        assertTrue(newMember2.isLead());
        assertEquals(now, newMember2.getStart());
        assertSame(newMember2, project.getMember(newLead));
    }

    @Test
    public void replaceLeadFuture() {
        Timestamp now = new Timestamp(System.currentTimeMillis());

        Employee sample = new Employee();
        SmallProject project = new SmallProject(null);

        List<Member> memberships = project.addMember(sample, now, true, now);

        assertNotNull(memberships);
        assertEquals(1, memberships.size());
        Member member = memberships.get(0);

        assertSame(sample, member.getEmployee());
        assertSame(project, member.getProject());
        assertEquals(now, member.getStart());
        assertNull(member.getEnd());

        assertEquals(1, project.getMembers().size());
        assertSame(member, project.getMember(sample));
    }
}
