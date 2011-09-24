The source for 2.0.2-glassfish-2 is from  https://svn.apache.org/repos/asf/myfaces/trinidad/trunk@1174527.

The following patch is applied:
===================================================================
--- trinidad-api/pom.xml	(revision 1174527)
+++ trinidad-api/pom.xml	(working copy)
@@ -25,9 +25,9 @@
   <description>Public API for the Apache MyFaces Trinidad project</description>
 
   <parent>
-    <groupId>org.apache.myfaces.trinidad</groupId>
+    <groupId>org.glassfish.external.trinidad</groupId>
     <artifactId>trinidad</artifactId>
-    <version>2.0.2-SNAPSHOT</version>
+    <version>2.0.2-glassfish-1</version>
   </parent>
 
   <artifactId>trinidad-api</artifactId>
@@ -35,11 +35,12 @@
   
   <dependencies>
 
-    <dependency>
-      <groupId>org.apache.myfaces.trinidad</groupId>
+     <dependency>
+      <groupId>org.glassfish.external.trinidad</groupId>
       <artifactId>trinidad-build</artifactId>
+      <scope>provided</scope> 
     </dependency>
-    
+
     <dependency>
         <groupId>org.apache.myfaces.buildtools</groupId>
         <artifactId>myfaces-builder-annotations</artifactId>

===================================================================
--- trinidad-impl/pom.xml	(revision 1174531)
+++ trinidad-impl/pom.xml	(working copy)
@@ -25,9 +25,9 @@
   <description>Private implementation of the Apache MyFaces Trinidad project</description>
 
   <parent>
-    <groupId>org.apache.myfaces.trinidad</groupId>
+    <groupId>org.glassfish.external.trinidad</groupId>
     <artifactId>trinidad</artifactId>
-    <version>2.0.2-SNAPSHOT</version>
+    <version>2.0.2-glassfish-1</version>
   </parent>
 
   <artifactId>trinidad-impl</artifactId>
@@ -73,17 +73,18 @@
     </dependency>
 
     <dependency>
+      <groupId>org.glassfish.external.trinidad</groupId>
+      <artifactId>trinidad-build</artifactId>
+      <scope>provided</scope> 
+    </dependency>
+
+    <dependency>
       <groupId>org.apache.myfaces.buildtools</groupId>
       <artifactId>myfaces-builder-annotations</artifactId>
     </dependency>
       
     <dependency>
-      <groupId>org.apache.myfaces.trinidad</groupId>
-      <artifactId>trinidad-build</artifactId>
-    </dependency>
-
-    <dependency>
-      <groupId>org.apache.myfaces.trinidad</groupId>
+      <groupId>org.glassfish.external.trinidad</groupId>
       <artifactId>trinidad-api</artifactId>
     </dependency>
 
@@ -94,7 +95,7 @@
 
     <!-- "test" scope dependencies -->
     <dependency>
-      <groupId>org.apache.myfaces.trinidad</groupId>
+      <groupId>org.glassfish.external.trinidad</groupId>
       <artifactId>trinidad-api</artifactId>
       <type>test-jar</type>
       <scope>test</scope>

