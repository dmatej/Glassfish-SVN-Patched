/*
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the "License").  You may not use this file except 
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at 
 * glassfish/bootstrap/legal/CDDLv1.0.txt or 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * See the License for the specific language governing 
 * permissions and limitations under the License.
 * 
 * When distributing Covered Code, include this CDDL 
 * HEADER in each file and include the License file at 
 * glassfish/bootstrap/legal/CDDLv1.0.txt.  If applicable, 
 * add the following below this CDDL HEADER, with the 
 * fields enclosed by brackets "[]" replaced with your 
 * own identifying information: Portions Copyright [yyyy] 
 * [name of copyright owner]
 */

/*
 * Copyright 2005 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at 
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */


package com.sun.org.apache.jdo.impl.enhancer.classfile;

import java.util.Stack;
import java.util.Vector;
import java.io.*;

/**
 * Subtype of ClassAttribute which describes the "Code" attribute
 * associated with a method.
 */
public class CodeAttribute extends ClassAttribute {
    public final static String expectedAttrName = "Code";

    /* The java class file contents defining this code attribute.
       If non-null, this must be disassembled before the remaining 
       fields of this instance may be accessed. */
    private byte theDataBytes[];

    /* The maximum number of stack entries used by this method */
    private int maxStack;

    /* The maximum number of local variables used by this method */
    private int maxLocals;

    /* The java VM byte code sequence for this method - null for native
       and abstract methods */
    private byte theCodeBytes[];

    /* The instruction sequence for this method - initially derived from
       the byte code array, but may later be modified */
    private Insn theCode;

    /* The exception ranges and handlers which apply to the code in this
       method */
    private ExceptionTable exceptionTable;

    /* The attributes which apply to this method */
    private AttributeVector codeAttributes;

    /* The method environment used for decompiling this code attribute */
    CodeEnv codeEnv;

    /* public accessors */

    /**
     * Return the maximum number of stack entries used by this method
     */
    public int stackUsed() {
        makeValid();
        return maxStack;
    }

    /**
     * Set the maximum number of stack entries used by this method
     */
    public void setStackUsed(int used) {
        makeValid();
        maxStack = used;
    }

    /**
     * Return the maximum number of local variables used by this method
     */
    public int localsUsed() {
        makeValid();
        return maxLocals;
    }

    /**
     * Set the maximum number of local variables used by this method
     */
    public void setLocalsUsed(int used) {
        makeValid();
        maxLocals = used;
    }

    /**
     * Return the java VM byte code sequence for this method - null for
     * native and abstract methods
     */
    public byte[] byteCodes() {
        makeValid();
        return theCodeBytes;
    }

    /**
     * Return the instruction sequence for this method - initially derived
     * from the byte code array, but may later be modified
     */
    public Insn theCode() {
        makeValid();
        if (theCode == null && codeEnv != null) {
            buildInstructions(codeEnv);
        }
        return theCode;
    }

    /**
     * Install the instruction sequence for this method - the byte code array
     * is later updated.
     */
    public void setTheCode(Insn insn) {
        makeValid();
        if (insn != null && insn.opcode() != Insn.opc_target)
            throw new InsnError(
                "The initial instruction in all methods must be a target");
        theCode = insn;
    }

    /**
     * Return the exception ranges and handlers which apply to the code in
     * this method.
     */
    public ExceptionTable exceptionHandlers() {
        makeValid();
        return exceptionTable;
    }

    /**
     * Return the attributes which apply to this code
     */
    public AttributeVector attributes() {
        makeValid();
        return codeAttributes;
    }

    /**
     * Constructs a CodeAttribute object for construction from scratch
     */
    public CodeAttribute(ConstUtf8 attrName,
                         int maxStack, int maxLocals,
                         Insn code, 
                         ExceptionTable excTable,
                         AttributeVector codeAttrs) {
        this(attrName, maxStack, maxLocals, code, null, /* byteCodes */
             excTable, codeAttrs, null /* CodeEnv */ );
    }

    /**
     * Constructs a CodeAttribute object 
     */
    public CodeAttribute(ConstUtf8 attrName,
                         int maxStack, int maxLocals,
                         Insn code, byte[] codeBytes,
                         ExceptionTable excTable,
                         AttributeVector codeAttrs,
                         CodeEnv codeEnv) {
        super(attrName);
        this.maxStack = maxStack;
        this.maxLocals = maxLocals;
        theCode = code;
        theCodeBytes = codeBytes;
        exceptionTable = excTable;
        codeAttributes = codeAttrs;
        this.codeEnv = codeEnv;
    }

    /**
     * Constructs a CodeAttribute object for later disassembly
     */
    public CodeAttribute(ConstUtf8 attrName, byte[] dataBytes, CodeEnv codeEnv) {
        super(attrName);
        this.theDataBytes = dataBytes;
        this.codeEnv = codeEnv;
    }

    /**
     * Compares this instance with another for structural equality.
     */
    //@olsen: added method
    public boolean isEqual(Stack msg, Object obj) {
        if (!(obj instanceof CodeAttribute)) {
            msg.push("obj/obj.getClass() = "
                     + (obj == null ? null : obj.getClass()));
            msg.push("this.getClass() = "
                     + this.getClass());
            return false;
        }
        CodeAttribute other = (CodeAttribute)obj;

        if (!super.isEqual(msg, other)) {
            return false;
        }

        if (this.stackUsed() != other.stackUsed()) {
            msg.push(String.valueOf("stackUsed() = "
                                    + other.stackUsed()));
            msg.push(String.valueOf("stackUsed() = "
                                    + this.stackUsed()));
            return false;
        }
        if (this.localsUsed() != other.localsUsed()) {
            msg.push(String.valueOf("localsUsed() = "
                                    + other.localsUsed()));
            msg.push(String.valueOf("localsUsed() = "
                                    + this.localsUsed()));
            return false;
        }

        // iterate over the instructions
        Insn theCode1 = this.theCode();
        Insn theCode2 = other.theCode();
        while (theCode1 != null && theCode2 != null) {
            // ignore targets (ignore line numbers)
            if (theCode1.opcode() == Insn.opc_target) {
                theCode1 = theCode1.next();
                continue;
            }
            if (theCode2.opcode() == Insn.opc_target) {
                theCode2 = theCode2.next();
                continue;
            }
            if (!theCode1.isEqual(msg, theCode2)) {
                msg.push("theCode()[i] = " + String.valueOf(theCode2));
                msg.push("theCode()[i] = " + String.valueOf(theCode1));
                return false;
            }
            theCode1 = theCode1.next();
            theCode2 = theCode2.next();
        }
        if (theCode1 == null ^ theCode2 == null) {
            msg.push("theCode()[i] = " + String.valueOf(theCode2));
            msg.push("theCode()[i] = " + String.valueOf(theCode1));
            return false;
        }

        if (!this.exceptionHandlers().isEqual(msg, other.exceptionHandlers())) {
            msg.push(String.valueOf("exceptionHandlers() = "
                                    + other.exceptionHandlers()));
            msg.push(String.valueOf("exceptionHandlers() = "
                                    + this.exceptionHandlers()));
            return false;
        }
        if (!this.attributes().isEqual(msg, other.attributes())) {
            msg.push(String.valueOf("attributes() = "
                                    + other.attributes()));
            msg.push(String.valueOf("attributes() = "
                                    + this.attributes()));
            return false;
        }
        return true;
    }

    /* package local methods */

    static CodeAttribute read(ConstUtf8 attrName,
                              DataInputStream data, ConstantPool pool)
        throws IOException {
        int maxStack = data.readUnsignedShort();
        int maxLocals = data.readUnsignedShort();
        int codeLength = data.readInt();
        byte codeBytes[] = new byte[codeLength];
        data.readFully(codeBytes);
        Insn code = null;
        CodeEnv codeEnv = new CodeEnv(pool);

        ExceptionTable excTable = ExceptionTable.read(data, codeEnv);
    
        AttributeVector codeAttrs = 
            AttributeVector.readAttributes(data, codeEnv);

        return new CodeAttribute(attrName, maxStack, maxLocals, code, codeBytes,
                                 excTable, codeAttrs, codeEnv);
    } 

    /* This version reads the attribute into a byte array for later 
       consumption */
    static CodeAttribute read(ConstUtf8 attrName, int attrLength,
                              DataInputStream data, ConstantPool pool)
        throws IOException {
        byte dataBytes[] = new byte[attrLength];
        data.readFully(dataBytes);
        return new CodeAttribute(attrName, dataBytes, new CodeEnv(pool));
    } 

    void write(DataOutputStream out) throws IOException {
        out.writeShort(attrName().getIndex());
        if (theDataBytes == null) {
            buildInstructionBytes();
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            DataOutputStream tmpOut = new DataOutputStream(baos);
            tmpOut.writeShort(maxStack);
            tmpOut.writeShort(maxLocals);
            tmpOut.writeInt(theCodeBytes.length);
            tmpOut.write(theCodeBytes, 0, theCodeBytes.length);
            exceptionTable.write(tmpOut);
            codeAttributes.write(tmpOut);

            tmpOut.flush();
            byte tmpBytes[] = baos.toByteArray();
            out.writeInt(tmpBytes.length);
            out.write(tmpBytes, 0, tmpBytes.length);
        } else {
            out.writeInt(theDataBytes.length);
            out.write(theDataBytes, 0, theDataBytes.length);
        }
    }

    void print(PrintStream out, int indent) {
        makeValid();
        ClassPrint.spaces(out, indent);
        out.print("Code:");
        out.print(" max_stack = " + Integer.toString(maxStack));
        out.print(" max_locals = " + Integer.toString(maxLocals));
        out.println(" Exceptions:");
        exceptionTable.print(out, indent+2);
        ClassPrint.spaces(out, indent);
        out.println("Code Attributes:");
        codeAttributes.print(out, indent+2);

        Insn insn = theCode();
        if (insn != null) {
            ClassPrint.spaces(out, indent);
            out.println("Instructions:");
            while (insn != null) {
                insn.print(out, indent+2);
                insn = insn.next();
            }
        }
    }

    /**
     *  Assign offsets to instructions and return the number of bytes.
     *  theCode must be non-null.
     */
    private int resolveOffsets() {
        Insn insn = theCode;
        int currPC = 0;
        while (insn != null) {
            currPC = insn.resolveOffset(currPC);
            insn = insn.next();
        }
        return currPC;
    }

    int codeSize() {
        makeValid();
        return theCodeBytes.length;
    }

    /**
     * Derive the instruction list from the instruction byte codes
     */
    private void buildInstructions(CodeEnv codeEnv) {
        if (theCodeBytes != null) {
            InsnReadEnv insnEnv = new InsnReadEnv(theCodeBytes, codeEnv);
            theCode = insnEnv.getTarget(0);
            Insn currInsn = theCode;

            /* First, create instructions */
            while (insnEnv.more()) {
                Insn newInsn = Insn.read(insnEnv);
                currInsn.setNext(newInsn);
                currInsn = newInsn;
            }

            /* Now, insert targets */
            InsnTarget targ;
            currInsn = theCode;
            Insn prevInsn = null;
            while (currInsn != null) {
                int off = currInsn.offset();

                /* We always insert a target a 0 to start so ignore that one */
                if (off > 0) {
                    targ = codeEnv.findTarget(off);
                    if (targ != null)
                        prevInsn.setNext(targ);
                }
                prevInsn = currInsn;
                currInsn = currInsn.next();
            }

            /* And follow up with a final target if needed */
            targ = codeEnv.findTarget(insnEnv.currentPC());
            if (targ != null)
                prevInsn.setNext(targ);
        }
    }

    /**
     * Derive the instruction byte codes from the instruction list
     * This should also recompute stack and variables but for now we
     * assume that this isn't needed
     */
    private void buildInstructionBytes() {
        if (theCode != null) {
            /* Make sure instructions have correct offsets */
            int size = resolveOffsets();
            theCodeBytes = new byte[size];

            Insn insn = theCode;
            int index = 0;
            while (insn != null) {
                index = insn.store(theCodeBytes, index);
                insn = insn.next();
            }
        }
    }

    /** If theDataBytes is non-null, disassemble this code attribute
     *  from the data bytes. */
    private void makeValid() {
        if (theDataBytes != null) {
            DataInputStream dis = new DataInputStream(
		new ByteArrayInputStream(theDataBytes));
            try {
                maxStack = dis.readUnsignedShort();
                maxLocals = dis.readUnsignedShort();
                int codeLength = dis.readInt();
                theCodeBytes = new byte[codeLength];
                dis.readFully(theCodeBytes);
                exceptionTable = ExceptionTable.read(dis, codeEnv);
                codeAttributes = AttributeVector.readAttributes(dis, codeEnv);
            } catch (java.io.IOException ioe) {
                throw new ClassFormatError("IOException while reading code attribute");
            }

            theDataBytes = null;
        }
    }
}

