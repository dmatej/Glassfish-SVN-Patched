schema2beans sources is from NetBeans workspace.
"hg clone http://hg.netbeans.org/main-silver/"

The above command will check-out the entire NetBeans workspace and it takes more than 3 hrs to do the check-out.  The sources mirrored to GlassFish repository contain only the modules used to build schema2beans.

To build:
1. cd nbbuild
2. ant bootstrap
3. cd ../schema1beans
4. ant 
