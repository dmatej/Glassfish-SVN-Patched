import javax.swing.DefaultListModel;
/*
 * JSetArrayDialog.java
 *
 * Created on April 28, 2006, 6:34 PM
 */

/**
 *
 * @author  kravtch
 */
import javax.swing.*;
import javax.swing.table.*;
import java.util.ArrayList;
import java.util.Vector;

public class JSetArrayDialog extends javax.swing.JDialog {
    ArrayList _values;
    String _attrName;
    NameHelper _nameHelper;
    DottedNameInfo _dottedNameInfo;
    /** Creates new form JSetArrayDialog */
    public JSetArrayDialog(java.awt.Frame parent, boolean modal, 
            NameHelper nameHelper, DottedNameInfo dottedNameInfo, 
            String attrName, Object[] values) {
        super(parent, modal);
        initComponents();
        this.setTitle(attrName + " Editor");
        _nameHelper = nameHelper;
        _dottedNameInfo = dottedNameInfo;
        _attrName = attrName;
        _values = new ArrayList();
        for(Object value:values)
        {
            _values.add(value);
        }
        fillArrayTable();
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
        jOkButton = new javax.swing.JButton();
        jCancelButton = new javax.swing.JButton();
        jScrollPane2 = new javax.swing.JScrollPane();
        jArrayTable = new javax.swing.JTable();
        jAddButton = new javax.swing.JButton();
        jRemoveButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setLocationByPlatform(true);
        setModal(true);
        jOkButton.setText("Save");
        jOkButton.setPreferredSize(new java.awt.Dimension(51, 23));
        jOkButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jOkButtonActionPerformed(evt);
            }
        });

        jCancelButton.setText("Cancel");
        jCancelButton.setPreferredSize(new java.awt.Dimension(51, 23));
        jCancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCancelButtonActionPerformed(evt);
            }
        });

        jArrayTable.setBackground(new java.awt.Color(255, 255, 204));
        jArrayTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane2.setViewportView(jArrayTable);

        jAddButton.setText("Add");
        jAddButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jAddButtonActionPerformed(evt);
            }
        });

        jRemoveButton.setText("Remove");
        jRemoveButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRemoveButtonActionPerformed(evt);
            }
        });

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(jScrollPane2, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 408, Short.MAX_VALUE)
                    .add(layout.createSequentialGroup()
                        .add(jAddButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 82, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                        .add(18, 18, 18)
                        .add(jRemoveButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 89, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                        .add(15, 15, 15)
                        .add(jOkButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 71, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED, 19, Short.MAX_VALUE)
                        .add(jCancelButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 71, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap())
        );

        layout.linkSize(new java.awt.Component[] {jAddButton, jCancelButton, jOkButton, jRemoveButton}, org.jdesktop.layout.GroupLayout.HORIZONTAL);

        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .add(20, 20, 20)
                .add(jScrollPane2, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 177, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                .add(20, 20, 20)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(jOkButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 23, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(jCancelButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(jRemoveButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 23, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(jAddButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 23, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jOkButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jOkButtonActionPerformed
        if(jArrayTable.isEditing()) {
            JOptionPane.showMessageDialog(this, 
                "Please complete table editing before submit", "",
                    JOptionPane.INFORMATION_MESSAGE);
            return;
        }
            
        DefaultTableModel model = ((DefaultTableModel)jArrayTable.getModel());
        Vector<Vector> vectors = model.getDataVector();
        String[] strs = new String[vectors.size()];
        for(int i=0; i<vectors.size(); i++)
        {
            strs[i] = (String)(vectors.elementAt(i).elementAt(0));
        }
        try {
            _nameHelper.setValue(_dottedNameInfo, 
                    (String)_attrName, 
                    (Object)strs, 
                    false);
        } catch (Exception e) {
            e.printStackTrace();
            JOptionPane.showMessageDialog(this, "Exception:\n"+e.getMessage(),"", JOptionPane.ERROR_MESSAGE);
//            fillArrayTable();
        }
    }//GEN-LAST:event_jOkButtonActionPerformed

    private void jCancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCancelButtonActionPerformed
        try {
            this.dispose();
        } catch (Throwable t)
        {
        }
    }//GEN-LAST:event_jCancelButtonActionPerformed

    private void jRemoveButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRemoveButtonActionPerformed
        int iSelected = jArrayTable.getSelectedRow();
        int nSelected = jArrayTable.getSelectedRowCount();
        for(int i=0; i<nSelected; i++)
        {
            ((DefaultTableModel)jArrayTable.getModel()).removeRow(iSelected);
        }
//        fillArrayTable();
        jArrayTable.changeSelection(iSelected, iSelected, false, false);
//        jArrayTable.getSelectionModel().setSelectionInterval(iSelected,iSelected);
    }//GEN-LAST:event_jRemoveButtonActionPerformed

    private void jAddButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jAddButtonActionPerformed
        _values.add("");
//        fillArrayTable();
        DefaultTableModel model = ((DefaultTableModel)jArrayTable.getModel());
        int nRows = model.getRowCount();
        model.addRow(new Object[]{""});
//jArrayTable.getSelectionModel().setSelectionInterval(iSelected,iSelected);
        jArrayTable.changeSelection(nRows, nRows, false, false);
    }//GEN-LAST:event_jAddButtonActionPerformed
    
    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new JSetArrayDialog(new javax.swing.JFrame(), 
                        true,
                        null, null,
                        "test",null).setVisible(true);
            }
        });
    }

    private void fillArrayTable()
    {
        fillArrayTable(jArrayTable, _attrName, _values);
    }
    
    private void fillArrayTable(JTable table, String header, ArrayList lines)
    {
        table.removeAll();
        DefaultTableModel model = new DefaultTableModel();
        model.addColumn(header);
        if(lines!=null)
        {
            for (Object line: lines)
            {
                model.addRow(new Object[]{line});
            }
        }
        table.setModel(model);
        table.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    }

    

    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jAddButton;
    private javax.swing.JTable jArrayTable;
    private javax.swing.JButton jCancelButton;
    private javax.swing.JButton jOkButton;
    private javax.swing.JButton jRemoveButton;
    private javax.swing.JScrollPane jScrollPane2;
    // End of variables declaration//GEN-END:variables
    
}
