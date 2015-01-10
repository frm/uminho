/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package habitat;

import controllers.ApplicationsController;
import controllers.Controller;
import controllers.ControllerFactory;
import data.DataException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import javax.swing.DefaultCellEditor;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import models.Activity;
import models.Application;
import models.Contact;
import models.Family;
import models.Question;
import models.Representative;
import models.SimpleMember;

/**
 *
 * @author tiago
 */
public class HabitatController extends javax.swing.JFrame {
    
    private Family currentFamily;
    private Representative currentRepresentative;
    private List<SimpleMember> currentMembers;
    private List<Contact> representativeContacts;
    private List<Application> familyApplications;
    private Application currentApplication;
    private String username;
    private String password;
    private int applicationIndex;
    /**
     * Creates new form GUI
     */
    public HabitatController(String username, String password) {
        this.password = password;
        this.username = username;
        this.setIconImage((new ImageIcon("etc/logo.png")).getImage());
        initComponents();
        listFamilies();
        
        familyList.getSelectionModel().addListSelectionListener(new ListSelectionListener(){
        public void valueChanged(ListSelectionEvent event) {
            String code;
            try {
                code = familyList.getValueAt(familyList.getSelectedRow(), 0).toString();
            } catch (Exception e) {
                return;
            }
            int i = Integer.parseInt(code);
            try {
                currentFamily = ControllerFactory.getFamiliesController().find(i);
                currentRepresentative = ControllerFactory.getRepresentativesController().findBy(
                    new HashMap<String, Object>() {{ put("familyID", currentFamily.getId());}}
            ).get(0);
                currentMembers = ControllerFactory.getMembersController().findBy(new HashMap<String, Object>() {{
                put("familyID", currentFamily.getId());
                }});
                
                representativeContacts = ControllerFactory.getContactsController().findBy(new HashMap<String, Object>() {{
                    put("OwnerType", "Representante");
                    put("Owner", currentRepresentative.getId());
                }});
                
                familyApplications = ControllerFactory.getApplicationsController().findBy(new HashMap<String, Object>() {{
                    put("familyId", currentFamily.getId());
                }});
                
                applicationIndex = familyApplications.size() - 1;
                
                if(applicationIndex > 0)
                    nextApplication.setEnabled(true);
                
                setCurrentFamily();
                setFamilyMembers();
                setCurrentApplication();
            } catch(DataException e) { }
        }
        });
        
        try {
            Collection<Activity> items = ControllerFactory.getActivityController().all();
            for( Activity a: items){
                mainWindowRepProf.addItem(a);
            }
        } catch (DataException ex) {
            JOptionPane.showMessageDialog(this, "Ocorreu um erro ao obter os dados");
        }
    }

    private boolean confirmPassword() {
        String password = JOptionPane.showInputDialog(this, "Por favor confirme password");
        return this.password.equals(password);
    }
    
    public void listFamilies() {
        List<Family> families  = new ArrayList<>();
        try {
            families = ControllerFactory.getFamiliesController().all();
            
            ((DefaultTableModel)familyList.getModel()).setRowCount(0);
            
            for(final Family f : families) {
                ((DefaultTableModel)familyList.getModel()).addRow(new Object[]{f.getId(), f.getName(), ControllerFactory.getRepresentativesController().findBy(
                    new HashMap<String, Object>() {{ put("familyID", f.getId());}}
                ).get(0).getName(), f.getAddress()});
            }
        } catch (DataException e) {
            JOptionPane.showMessageDialog(this, "Erro a ler dados");
        }
    }
    
    public void setCurrentApplication() {
        ((DefaultTableModel)applicationQuestionnaire.getModel()).setRowCount(0);
        applicationDate.setText("");
        applicationApproved.setSelected(false);
        applicationPriority.setSelectedIndex(-1);
        applicationLocation.setText("");
        applicationApprovalDate.setText("");
        applicationId.setText("");
        applicationNotes.setText("");
        previousApplication.setEnabled(false);
        nextApplication.setEnabled(false);
        applicationPages.setText("");
        
        if (familyApplications.size() > 0) {
            currentApplication = familyApplications.get(applicationIndex);
        } else
            return;
        
        Controller<Question> qc = ControllerFactory.getQuestionsController();
        try {
            List<Question> activeQuestions = qc.findBy(new HashMap<String, Object>() {{ put("enabled", true); }});
            for(Question q : activeQuestions)
                ((DefaultTableModel)applicationQuestionnaire.getModel()).addRow(new Object[]{q,""});            
        } catch(DataException e) {
            JOptionPane.showMessageDialog(this, "Erro a ler dados");
        }
        
        int totalPages = familyApplications.size();
        applicationPages.setText(new StringBuilder().append(totalPages - applicationIndex).append("/")
                                            .append(totalPages).toString());
        
        currentApplication = familyApplications.get(applicationIndex);
        
        applicationDate.setText(Util.dateToStr(currentApplication.getApplicationDate()));
        applicationApproved.setSelected(currentApplication.getStatus());
        applicationPriority.setSelectedIndex(currentApplication.getPriority());
        applicationLocation.setText(currentApplication.getLocation());
        applicationApprovalDate.setText(Util.dateToStr(currentApplication.getApprovalDate()));
        applicationId.setText(Integer.toString(currentApplication.getId()));
        applicationNotes.setText(currentApplication.getNotes());
    }
    
    public void setCurrentFamily() {
        Family f = currentFamily;
        familyName.setText(f.getName());
        familyAddress.setText(f.getAddress());
        familyIncome.setText(Float.toString(f.getIncome()));
        familyApproved.setSelected(f.getApproved());
        familyNotes.setText(f.getObservations());        
        Representative r = currentRepresentative;
        
        familyRep.setText(r.getName());
        repBirthDate.setText(Util.dateToStr(r.getBirthDate()));
        repMaritalStatus.setSelectedItem(r.getMaritalStatus());
        repEducation.setText(r.getEducation());
        repNif.setText(r.getNif());
        repNib.setText(r.getNib());
        mainWindowRepProf.setSelectedItem(currentRepresentative.getActivity());
        repBirthPlace.setText(r.getBirthPlace());
        repNationality.setSelectedItem(r.getNationality());
        
        ((DefaultTableModel)repContacts.getModel()).setRowCount(0);
            
        for(Contact c : representativeContacts)
            ((DefaultTableModel)repContacts.getModel()).addRow(new Object[]{c.getType(), c.getValue()});
    }
    
    public void setFamilyMembers() {
        try{
            final Family f = currentFamily;
            List<SimpleMember> members = currentMembers;

            ((DefaultTableModel)memberList.getModel()).setRowCount(0);

            for(final SimpleMember m : members)
                ((DefaultTableModel)memberList.getModel()).addRow(new Object[]{ m.getId(), m.getName(), Util.dateToStr(m.getBirthDate()), m.getKinship() });
        } catch (NullPointerException e){}
        
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel4 = new javax.swing.JPanel();
        jDialog1 = new javax.swing.JDialog();
        jFormattedTextField1 = new javax.swing.JFormattedTextField();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        jPanel1 = new javax.swing.JPanel();
        jPanel33 = new javax.swing.JPanel();
        familySubTabbedPane = new javax.swing.JTabbedPane();
        jPanel34 = new javax.swing.JPanel();
        jLabel58 = new javax.swing.JLabel();
        jLabel59 = new javax.swing.JLabel();
        jLabel60 = new javax.swing.JLabel();
        jLabel61 = new javax.swing.JLabel();
        jLabel62 = new javax.swing.JLabel();
        jSeparator15 = new javax.swing.JSeparator();
        jLabel63 = new javax.swing.JLabel();
        jLabel64 = new javax.swing.JLabel();
        jLabel65 = new javax.swing.JLabel();
        jLabel66 = new javax.swing.JLabel();
        jLabel88 = new javax.swing.JLabel();
        editFamily = new javax.swing.JButton();
        deleteFamily = new javax.swing.JButton();
        jLabel8 = new javax.swing.JLabel();
        jLabel16 = new javax.swing.JLabel();
        jLabel20 = new javax.swing.JLabel();
        jScrollPane14 = new javax.swing.JScrollPane();
        familyNotes = new javax.swing.JTextArea();
        familyName = new javax.swing.JTextField();
        familyAddress = new javax.swing.JTextField();
        familyIncome = new javax.swing.JFormattedTextField();
        familyApproved = new javax.swing.JCheckBox();
        jScrollPane15 = new javax.swing.JScrollPane();
        repContacts = new javax.swing.JTable();
        familyRep = new javax.swing.JTextField();
        repBirthDate = new javax.swing.JFormattedTextField();
        repEducation = new javax.swing.JTextField();
        repNif = new javax.swing.JTextField();
        repNib = new javax.swing.JTextField();
        jLabel67 = new javax.swing.JLabel();
        jLabel44 = new javax.swing.JLabel();
        deleteRepContact = new javax.swing.JButton();
        addRepContact = new javax.swing.JButton();
        jLabel28 = new javax.swing.JLabel();
        cancelEditFamily = new javax.swing.JButton();
        submitEditFamily = new javax.swing.JButton();
        repMaritalStatus = new javax.swing.JComboBox();
        jLabel19 = new javax.swing.JLabel();
        repBirthPlace = new javax.swing.JTextField();
        jLabel22 = new javax.swing.JLabel();
        repNationality = new javax.swing.JComboBox();
        mainWindowRepProf = new javax.swing.JComboBox();
        jLabel68 = new javax.swing.JLabel();
        familyVolHours = new javax.swing.JTextField();
        jPanel35 = new javax.swing.JPanel();
        addApplication = new javax.swing.JButton();
        jLabel23 = new javax.swing.JLabel();
        jLabel24 = new javax.swing.JLabel();
        jLabel25 = new javax.swing.JLabel();
        jLabel26 = new javax.swing.JLabel();
        jLabel27 = new javax.swing.JLabel();
        previousApplication = new javax.swing.JButton();
        nextApplication = new javax.swing.JButton();
        deleteApplication = new javax.swing.JButton();
        editApplication = new javax.swing.JButton();
        jLabel5 = new javax.swing.JLabel();
        jLabel21 = new javax.swing.JLabel();
        jLabel29 = new javax.swing.JLabel();
        applicationPages = new javax.swing.JLabel();
        jScrollPane7 = new javax.swing.JScrollPane();
        applicationQuestionnaire = new javax.swing.JTable();
        applicationDate = new javax.swing.JFormattedTextField();
        applicationLocation = new javax.swing.JTextField();
        applicationApprovalDate = new javax.swing.JFormattedTextField();
        applicationId = new javax.swing.JTextField();
        jScrollPane9 = new javax.swing.JScrollPane();
        applicationNotes = new javax.swing.JTextArea();
        editQuestionnaireSubmit = new javax.swing.JButton();
        applicationPriority = new javax.swing.JComboBox();
        applicationApproved = new javax.swing.JCheckBox();
        editQuestionnaireCancel = new javax.swing.JButton();
        jPanel36 = new javax.swing.JPanel();
        submitMembers = new javax.swing.JButton();
        removeMember = new javax.swing.JButton();
        addMember = new javax.swing.JButton();
        jScrollPane18 = new javax.swing.JScrollPane();
        memberList = new javax.swing.JTable();
        addFamily = new javax.swing.JButton();
        jScrollPane6 = new javax.swing.JScrollPane();
        familyList = new javax.swing.JTable();
        jPanel2 = new javax.swing.JPanel();
        jPanel45 = new javax.swing.JPanel();
        jTabbedPane15 = new javax.swing.JTabbedPane();
        jPanel46 = new javax.swing.JPanel();
        jLabel121 = new javax.swing.JLabel();
        jLabel122 = new javax.swing.JLabel();
        jLabel123 = new javax.swing.JLabel();
        jLabel124 = new javax.swing.JLabel();
        jScrollPane13 = new javax.swing.JScrollPane();
        projectNotes = new javax.swing.JTextArea();
        jLabel17 = new javax.swing.JLabel();
        jLabel35 = new javax.swing.JLabel();
        jLabel36 = new javax.swing.JLabel();
        jLabel37 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        editProject = new javax.swing.JButton();
        deleteProject = new javax.swing.JButton();
        jLabel33 = new javax.swing.JLabel();
        jtfPDEC = new javax.swing.JFormattedTextField();
        projectSignDate = new javax.swing.JFormattedTextField();
        jtfPDF = new javax.swing.JFormattedTextField();
        projectEta = new javax.swing.JFormattedTextField();
        projectStartDate = new javax.swing.JFormattedTextField();
        cancelEditProject = new javax.swing.JButton();
        submitEditProject = new javax.swing.JButton();
        projectBudget = new javax.swing.JFormattedTextField();
        projectFinalCost = new javax.swing.JFormattedTextField();
        jtfPVP = new javax.swing.JFormattedTextField();
        jLabel46 = new javax.swing.JLabel();
        jLabel47 = new javax.swing.JLabel();
        jLabel48 = new javax.swing.JLabel();
        projectName = new javax.swing.JTextField();
        jPanel10 = new javax.swing.JPanel();
        jLabel32 = new javax.swing.JLabel();
        jScrollPane11 = new javax.swing.JScrollPane();
        paymentPlanNotes = new javax.swing.JTextArea();
        addPayment = new javax.swing.JButton();
        deletePayment = new javax.swing.JButton();
        editPaymentPlan = new javax.swing.JButton();
        jScrollPane12 = new javax.swing.JScrollPane();
        paymentPlan = new javax.swing.JTable();
        jLabel31 = new javax.swing.JLabel();
        jPanel6 = new javax.swing.JPanel();
        addTask = new javax.swing.JButton();
        removeTask = new javax.swing.JButton();
        jScrollPane22 = new javax.swing.JScrollPane();
        taskList = new javax.swing.JTable();
        taskViewDetails = new javax.swing.JButton();
        addProject = new javax.swing.JButton();
        jScrollPane16 = new javax.swing.JScrollPane();
        projectList = new javax.swing.JTable();
        jPanel3 = new javax.swing.JPanel();
        jPanel24 = new javax.swing.JPanel();
        jTabbedPane12 = new javax.swing.JTabbedPane();
        jPanel25 = new javax.swing.JPanel();
        jLabel110 = new javax.swing.JLabel();
        jLabel111 = new javax.swing.JLabel();
        jLabel112 = new javax.swing.JLabel();
        jLabel114 = new javax.swing.JLabel();
        jScrollPane8 = new javax.swing.JScrollPane();
        volunteerNotes = new javax.swing.JTextArea();
        jLabel9 = new javax.swing.JLabel();
        jLabel10 = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        jLabel12 = new javax.swing.JLabel();
        jLabel13 = new javax.swing.JLabel();
        jLabel14 = new javax.swing.JLabel();
        jLabel15 = new javax.swing.JLabel();
        editVolunteer = new javax.swing.JButton();
        deleteVolunteer = new javax.swing.JButton();
        volunteerName = new javax.swing.JTextField();
        volunteerAddress = new javax.swing.JTextField();
        volunteerTeam = new javax.swing.JComboBox();
        addVolunteerTeam = new javax.swing.JButton();
        volunteerBirthDate = new javax.swing.JFormattedTextField();
        volunteerEducation = new javax.swing.JTextField();
        volunteerBirthPlace = new javax.swing.JTextField();
        volunteerMaritalStatus = new javax.swing.JComboBox();
        volunteerNationality = new javax.swing.JComboBox();
        volunteerNib = new javax.swing.JTextField();
        volunteerNif = new javax.swing.JTextField();
        cancelEditVolunteer = new javax.swing.JButton();
        submitEditVolunteer = new javax.swing.JButton();
        jPanel11 = new javax.swing.JPanel();
        volunteerViewEventDetails = new javax.swing.JButton();
        jScrollPane19 = new javax.swing.JScrollPane();
        volunteerEventList = new javax.swing.JTable();
        jPanel12 = new javax.swing.JPanel();
        volunteerViewProjectDetails = new javax.swing.JButton();
        jScrollPane20 = new javax.swing.JScrollPane();
        jTable8 = new javax.swing.JTable();
        addVolunteer = new javax.swing.JButton();
        jScrollPane10 = new javax.swing.JScrollPane();
        jTable3 = new javax.swing.JTable();
        jPanel5 = new javax.swing.JPanel();
        jPanel43 = new javax.swing.JPanel();
        jTabbedPane14 = new javax.swing.JTabbedPane();
        jPanel44 = new javax.swing.JPanel();
        jLabel115 = new javax.swing.JLabel();
        jLabel130 = new javax.swing.JLabel();
        jLabel131 = new javax.swing.JLabel();
        jLabel133 = new javax.swing.JLabel();
        jScrollPane32 = new javax.swing.JScrollPane();
        eventObservation = new javax.swing.JTextArea();
        editEventButton = new javax.swing.JButton();
        removeEventButton = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        eventDate = new javax.swing.JFormattedTextField();
        eventAddress = new javax.swing.JTextField();
        eventRaisedValue = new javax.swing.JFormattedTextField();
        cancelEditEventButton = new javax.swing.JButton();
        submitEditEventButton = new javax.swing.JButton();
        jLabel4 = new javax.swing.JLabel();
        eventParticipantNmb = new javax.swing.JSpinner();
        jPanel7 = new javax.swing.JPanel();
        jScrollPane17 = new javax.swing.JScrollPane();
        participantsTable = new javax.swing.JTable();
        editParticipantsButton = new javax.swing.JButton();
        addEventButton = new javax.swing.JButton();
        jScrollPane21 = new javax.swing.JScrollPane();
        eventTable = new javax.swing.JTable();
        jPanel8 = new javax.swing.JPanel();
        jPanel47 = new javax.swing.JPanel();
        jTabbedPane16 = new javax.swing.JTabbedPane();
        jPanel48 = new javax.swing.JPanel();
        jLabel125 = new javax.swing.JLabel();
        jLabel126 = new javax.swing.JLabel();
        jLabel127 = new javax.swing.JLabel();
        jLabel128 = new javax.swing.JLabel();
        jLabel18 = new javax.swing.JLabel();
        jLabel42 = new javax.swing.JLabel();
        editDonorButton = new javax.swing.JButton();
        removeDonorButton = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jScrollPane3 = new javax.swing.JScrollPane();
        donorObservation = new javax.swing.JTextArea();
        donorName = new javax.swing.JTextField();
        donorAddress = new javax.swing.JTextField();
        donorNIF = new javax.swing.JTextField();
        donorActivity = new javax.swing.JTextField();
        donorType = new javax.swing.JTextField();
        jScrollPane23 = new javax.swing.JScrollPane();
        donorContacts = new javax.swing.JTable();
        addDonorContact = new javax.swing.JButton();
        deleteDonorContact = new javax.swing.JButton();
        cancelDonorEdit = new javax.swing.JButton();
        submitDonorEdit = new javax.swing.JButton();
        jPanel9 = new javax.swing.JPanel();
        jScrollPane24 = new javax.swing.JScrollPane();
        addDonationTable = new javax.swing.JTable();
        removeDonationButton = new javax.swing.JButton();
        addDonationButton = new javax.swing.JButton();
        confirmRemoveDonationButton = new javax.swing.JButton();
        cancelRemoveDonationButton = new javax.swing.JButton();
        addDonorButton = new javax.swing.JButton();
        jButton40 = new javax.swing.JButton();
        jScrollPane25 = new javax.swing.JScrollPane();
        jTable2 = new javax.swing.JTable();
        jPanel13 = new javax.swing.JPanel();
        jLabel7 = new javax.swing.JLabel();
        search = new javax.swing.JTextField();
        jLabel6 = new javax.swing.JLabel();
        clearSearch = new javax.swing.JButton();

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jDialog1Layout = new javax.swing.GroupLayout(jDialog1.getContentPane());
        jDialog1.getContentPane().setLayout(jDialog1Layout);
        jDialog1Layout.setHorizontalGroup(
            jDialog1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 400, Short.MAX_VALUE)
        );
        jDialog1Layout.setVerticalGroup(
            jDialog1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 300, Short.MAX_VALUE)
        );

        jFormattedTextField1.setText("jFormattedTextField1");

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Habitat");
        setBackground(new java.awt.Color(54, 79, 194));
        setResizable(false);

        jTabbedPane1.setBackground(new java.awt.Color(22, 113, 204));
        jTabbedPane1.setFont(new java.awt.Font("Calibri Light", 0, 18)); // NOI18N
        jTabbedPane1.setMinimumSize(new java.awt.Dimension(500, 245));
        jTabbedPane1.setOpaque(true);
        jTabbedPane1.setPreferredSize(new java.awt.Dimension(312, 463));

        jPanel33.setOpaque(false);

        familySubTabbedPane.setBackground(new java.awt.Color(255, 255, 255));
        familySubTabbedPane.setTabPlacement(javax.swing.JTabbedPane.LEFT);
        familySubTabbedPane.setFont(new java.awt.Font("Calibri Light", 0, 14)); // NOI18N
        familySubTabbedPane.setPreferredSize(new java.awt.Dimension(811, 590));
        familySubTabbedPane.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                if(familySubTabbedPane.getSelectedIndex() == 1)
                setCurrentApplication();
                else if(familySubTabbedPane.getSelectedIndex() == 2)
                setFamilyMembers();
            }
        });

        jPanel34.setBackground(new java.awt.Color(255, 255, 255));

        jLabel58.setText("Nome");

        jLabel59.setText("Morada");

        jLabel60.setText("Candidatura Aprovada");

        jLabel61.setText("Rendimento");

        jLabel62.setText("Observações");

        jLabel63.setFont(new java.awt.Font("Calibri Light", 0, 24)); // NOI18N
        jLabel63.setText("Família");

        jLabel64.setText("Nome");

        jLabel65.setText("Data de Nasc.");

        jLabel66.setText("Escolaridade");

        jLabel88.setText("Profissão");

        editFamily.setBackground(new java.awt.Color(22, 113, 204));
        editFamily.setForeground(new java.awt.Color(255, 255, 255));
        editFamily.setText("Editar");
        editFamily.setBorderPainted(false);
        editFamily.setContentAreaFilled(false);
        editFamily.setOpaque(true);
        editFamily.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                editFamilyActionPerformed(evt);
            }
        });

        deleteFamily.setBackground(new java.awt.Color(22, 113, 204));
        deleteFamily.setForeground(new java.awt.Color(255, 255, 255));
        deleteFamily.setText("Remover");
        deleteFamily.setBorderPainted(false);
        deleteFamily.setContentAreaFilled(false);
        deleteFamily.setOpaque(true);
        deleteFamily.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteFamilyActionPerformed(evt);
            }
        });

        jLabel8.setText("Estado Civil");

        jLabel16.setText("NIF");

        jLabel20.setText("NIB");

        jScrollPane14.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

        familyNotes.setEditable(false);
        familyNotes.setColumns(20);
        familyNotes.setLineWrap(true);
        familyNotes.setRows(5);
        familyNotes.setToolTipText("");
        familyNotes.setWrapStyleWord(true);
        jScrollPane14.setViewportView(familyNotes);
        familyNotes.setDocument(new JTextAreaLimit(500));

        familyName.setEditable(false);

        familyAddress.setEditable(false);

        familyIncome.setEditable(false);
        familyIncome.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00; #0.00"))));

        familyApproved.setBackground(new java.awt.Color(255, 255, 255));
        familyApproved.setBorder(null);
        familyApproved.setEnabled(false);

        repContacts.setAutoCreateRowSorter(true);
        repContacts.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {

            },
            new String [] {
                "Tipo", "Contacto"
            }
        ) {
            Class[] types = new Class [] {
                java.lang.String.class, java.lang.String.class
            };

            public Class getColumnClass(int columnIndex) {
                return types [columnIndex];
            }
        });
        repContacts.setEnabled(false);
        repContacts.getTableHeader().setReorderingAllowed(false);
        jScrollPane15.setViewportView(repContacts);
        if (repContacts.getColumnModel().getColumnCount() > 0) {
            repContacts.getColumnModel().getColumn(0).setCellEditor(new DefaultCellEditor(
                new javax.swing.JComboBox(
                    new javax.swing.DefaultComboBoxModel(
                        new String[] { "Telefone", "Telemóvel", "Email", "Fax" }))));
    }

    familyRep.setEditable(false);

    repBirthDate.setEditable(false);
    repBirthDate.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.DateFormatter(new java.text.SimpleDateFormat("dd/MM/yyyy"))));
    repBirthDate.setToolTipText("dd/MM/aaaa");
    repBirthDate.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            repBirthDateActionPerformed(evt);
        }
    });

    repEducation.setEditable(false);

    repNif.setEditable(false);

    repNib.setEditable(false);

    jLabel67.setFont(new java.awt.Font("Calibri Light", 0, 24)); // NOI18N
    jLabel67.setText("Representante");

    jLabel44.setText("€");

    deleteRepContact.setBackground(new java.awt.Color(22, 113, 204));
    deleteRepContact.setFont(new java.awt.Font("Noto Sans", 0, 14)); // NOI18N
    deleteRepContact.setForeground(new java.awt.Color(254, 254, 254));
    deleteRepContact.setText("-");
    deleteRepContact.setBorderPainted(false);
    deleteRepContact.setContentAreaFilled(false);
    deleteRepContact.setMaximumSize(new java.awt.Dimension(41, 23));
    deleteRepContact.setMinimumSize(new java.awt.Dimension(41, 23));
    deleteRepContact.setOpaque(true);
    deleteRepContact.setPreferredSize(new java.awt.Dimension(22, 40));
    deleteRepContact.setVisible(false);
    deleteRepContact.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            deleteRepContactActionPerformed(evt);
        }
    });

    addRepContact.setBackground(new java.awt.Color(22, 113, 204));
    addRepContact.setFont(new java.awt.Font("Noto Sans", 0, 18)); // NOI18N
    addRepContact.setForeground(new java.awt.Color(254, 254, 254));
    addRepContact.setText("+");
    addRepContact.setBorderPainted(false);
    addRepContact.setContentAreaFilled(false);
    addRepContact.setOpaque(true);
    addRepContact.setVisible(false);
    addRepContact.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addRepContactActionPerformed(evt);
        }
    });

    jLabel28.setText("Contactos");

    cancelEditFamily.setBackground(new java.awt.Color(22, 113, 204));
    cancelEditFamily.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    cancelEditFamily.setForeground(new java.awt.Color(255, 255, 255));
    cancelEditFamily.setText("Cancelar");
    cancelEditFamily.setBorderPainted(false);
    cancelEditFamily.setContentAreaFilled(false);
    cancelEditFamily.setOpaque(true);
    cancelEditFamily.setVisible(false);
    cancelEditFamily.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            cancelEditFamilyActionPerformed(evt);
        }
    });

    submitEditFamily.setBackground(new java.awt.Color(22, 113, 204));
    submitEditFamily.setForeground(new java.awt.Color(255, 255, 255));
    submitEditFamily.setText("Submeter");
    submitEditFamily.setBorderPainted(false);
    submitEditFamily.setOpaque(true);
    submitEditFamily.setVisible(false);
    submitEditFamily.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            submitEditFamilyActionPerformed(evt);
        }
    });

    repMaritalStatus.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Solteiro(a)", "Casado(a)", "Divorciado(a)", "Viúvo(a)" }));
    repMaritalStatus.setEnabled(false);

    jLabel19.setText("Naturalidade");

    repBirthPlace.setEditable(false);

    jLabel22.setText("Nacionalidade");

    repNationality.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Afeganistão", "África do Sul", "Albânia", "Alemanha", "Andorra", "Angola", "Arábia Saudita", "Argélia", "Argentina", "Armênia", "Austrália", "Áustria", "Azerbaijão", "Bahamas", "Bahrein", "Bangladesh", "Barbados", "Bélgica", "Belize", "Benin", "Bielorrússia", "Bolívia", "Bósnia", "Botsuana", "Brasil", "Brunei", "Bulgária", "Burkina-Fasso", "Burundi", "Butão", "Cabo Verde", "Camarões", "Camboja", "Canadá", "Catar", "Cazaquistão", "Chade", "Chile", "China", "Chipre", "Cingapura", "Colômbia", "Comores", "Congo", "Coréia do Norte", "Coréia do Sul", "Costado Marfim", "Costa Rica", "Croácia", "Cuba", "Dinamarca", "Djibuti", "Dominica", "Egito", "El Salvador", "Emirados Árabes Unidos", "Equador", "Eritreia", "Escócia", "Eslováquia", "Eslovênia", "Espanha", "Estados Unidos", "Estônia", "Etiópia", "Federação Russa", "Fiji", "Filipinas", "Finlândia", "Formosa Taiwan", "França", "Gabão", "Gâmbia", "Gana", "Geórgia", "Grã-Bretanha", "Granada", "Grécia", "Groenlândia", "Guatemala", "Guiana", "Guiana Francesa", "Guiné", "Guiné Bissau", "Guiné Equatorial", "Haiti", "Holanda", "Honduras", "Hungria", "Iêmen", "Ilhas Marshall", "Ilhas Salomão", "Índia", "Indonésia", "Irão", "Iraque", "Irlanda", "Irlanda do Norte", "Islândia", "Israel", "Itália", "Jamaica", "Japão", "Jordânia", "Kiribati", "Kuwait", "Laos", "Lesoto", "Letônia", "Líbano", "Libéria", "Líbia", "Liechtenstein", "Lituânia", "Luxemburgo", "Macedônia", "Madagascar", "Malásia", "Malauí", "Maldivas", "Mali", "Malta", "Marrocos", "Maurício", "Mauritânia", "México", "Mianmar", "Micronésia", "Moçambique", "Moldávia", "Mônaco", "Mongólia", "Namíbia", "Nauru", "Nepal", "Nicarágua", "Níger", "Nigéria", "Noruega", "Nova Zelândia", "Omã", "Palau", "Panamá", "Papua Nova Guiné", "Paquistão", "Paraguai", "Peru", "Polônia", "Porto Rico", "Portugal", "Quênia", "Quirguistão", "Reino Unido", "Rep.Centro-Africana", "Rep.Dominicana", "República Tcheca", "Romênia", "Ruanda", "Samoa", "SanMarino", "Santa Lúcia", "São Cristóvão e Névis", "São Tomé e Príncipe", "São Vicente e Granadinas", "Seicheles", "Senegal", "Serra Leoa", "Sérvia e Montenegro", "Síria", "Somália", "Sri-Lanka", "Suazilândia", "Sudão", "Suécia", "Suáça", "Suriname", "Tadjiquistão", "Tailândia", "Tanzânia", "Togo", "Tonga", "Trinidade Tobago", "Tunísia", "Turcomenistão", "Turquia", "Tuvalu", "Ucrânia", "Uganda", "Uruguai", "Uzbequistão", "Vanuatu", "Vaticano", "Venezuela", "Vietnã", "Zaire", "Zâmbia", "Zimbábue " }));
    repNationality.setSelectedIndex(149);
    repNationality.setEnabled(false);
    repNationality.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            repNationalityActionPerformed(evt);
        }
    });

    mainWindowRepProf.setEnabled(false);

    jLabel68.setText("Horas de Voluntariado");

    familyVolHours.setEditable(false);

    javax.swing.GroupLayout jPanel34Layout = new javax.swing.GroupLayout(jPanel34);
    jPanel34.setLayout(jPanel34Layout);
    jPanel34Layout.setHorizontalGroup(
        jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel34Layout.createSequentialGroup()
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel34Layout.createSequentialGroup()
                    .addGap(22, 22, 22)
                    .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                        .addComponent(jLabel66)
                        .addComponent(jLabel65)
                        .addComponent(jLabel88)
                        .addComponent(jLabel64)))
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel34Layout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jLabel28, javax.swing.GroupLayout.Alignment.TRAILING)
                        .addComponent(jLabel19, javax.swing.GroupLayout.Alignment.TRAILING)
                        .addComponent(jLabel22, javax.swing.GroupLayout.Alignment.TRAILING))))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel34Layout.createSequentialGroup()
                    .addComponent(jLabel67)
                    .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addGroup(jPanel34Layout.createSequentialGroup()
                    .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                        .addGroup(jPanel34Layout.createSequentialGroup()
                            .addComponent(repNationality, javax.swing.GroupLayout.PREFERRED_SIZE, 199, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(addRepContact, javax.swing.GroupLayout.PREFERRED_SIZE, 31, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(deleteRepContact, javax.swing.GroupLayout.PREFERRED_SIZE, 31, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addComponent(familyRep)
                        .addComponent(jScrollPane15)
                        .addGroup(jPanel34Layout.createSequentialGroup()
                            .addComponent(cancelEditFamily)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(submitEditFamily)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(editFamily)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(deleteFamily))
                        .addGroup(jPanel34Layout.createSequentialGroup()
                            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                .addGroup(jPanel34Layout.createSequentialGroup()
                                    .addComponent(repBirthDate, javax.swing.GroupLayout.PREFERRED_SIZE, 71, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jLabel8))
                                .addGroup(jPanel34Layout.createSequentialGroup()
                                    .addComponent(repBirthPlace, javax.swing.GroupLayout.PREFERRED_SIZE, 200, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addGap(0, 0, Short.MAX_VALUE))
                                .addGroup(jPanel34Layout.createSequentialGroup()
                                    .addGap(0, 0, Short.MAX_VALUE)
                                    .addComponent(jLabel16))
                                .addGroup(jPanel34Layout.createSequentialGroup()
                                    .addComponent(mainWindowRepProf, javax.swing.GroupLayout.PREFERRED_SIZE, 200, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jLabel20)))
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                .addComponent(repNib, javax.swing.GroupLayout.DEFAULT_SIZE, 199, Short.MAX_VALUE)
                                .addComponent(repMaritalStatus, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addComponent(repNif)))
                        .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanel34Layout.createSequentialGroup()
                            .addComponent(repEducation, javax.swing.GroupLayout.PREFERRED_SIZE, 200, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGap(0, 0, Short.MAX_VALUE)))
                    .addGap(33, 33, 33))))
        .addGroup(jPanel34Layout.createSequentialGroup()
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel34Layout.createSequentialGroup()
                    .addGap(19, 19, 19)
                    .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                        .addGroup(jPanel34Layout.createSequentialGroup()
                            .addComponent(jLabel62)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jScrollPane14, javax.swing.GroupLayout.PREFERRED_SIZE, 489, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGroup(jPanel34Layout.createSequentialGroup()
                            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                .addComponent(jLabel61)
                                .addComponent(jLabel59)
                                .addComponent(jLabel58))
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                .addComponent(familyName, javax.swing.GroupLayout.DEFAULT_SIZE, 484, Short.MAX_VALUE)
                                .addComponent(jLabel63)
                                .addGroup(jPanel34Layout.createSequentialGroup()
                                    .addComponent(familyApproved)
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jLabel60))
                                .addGroup(jPanel34Layout.createSequentialGroup()
                                    .addComponent(familyIncome, javax.swing.GroupLayout.PREFERRED_SIZE, 59, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jLabel44)
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jLabel68)
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(familyVolHours, javax.swing.GroupLayout.PREFERRED_SIZE, 62, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addComponent(familyAddress)))))
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel34Layout.createSequentialGroup()
                    .addGap(0, 0, Short.MAX_VALUE)
                    .addComponent(jSeparator15, javax.swing.GroupLayout.PREFERRED_SIZE, 615, javax.swing.GroupLayout.PREFERRED_SIZE)))
            .addContainerGap())
    );
    jPanel34Layout.setVerticalGroup(
        jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel34Layout.createSequentialGroup()
            .addGap(17, 17, 17)
            .addComponent(jLabel63)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel58)
                .addComponent(familyName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGap(6, 6, 6)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel59)
                .addComponent(familyAddress, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel61)
                .addComponent(familyIncome, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel44)
                .addComponent(jLabel68)
                .addComponent(familyVolHours, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGap(18, 18, 18)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                .addComponent(familyApproved)
                .addComponent(jLabel60))
            .addGap(21, 21, 21)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jScrollPane14, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel62))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(jSeparator15, javax.swing.GroupLayout.PREFERRED_SIZE, 16, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(jLabel67)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel64)
                .addComponent(familyRep, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel65)
                .addComponent(jLabel8)
                .addComponent(repBirthDate, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(repMaritalStatus, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel66)
                .addComponent(jLabel16)
                .addComponent(repEducation, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(repNif, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel88)
                .addComponent(jLabel20)
                .addComponent(repNib, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(mainWindowRepProf, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel19)
                .addComponent(repBirthPlace, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel22)
                        .addComponent(repNationality, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addComponent(addRepContact, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.PREFERRED_SIZE, 23, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addComponent(deleteRepContact, javax.swing.GroupLayout.PREFERRED_SIZE, 23, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jScrollPane15, javax.swing.GroupLayout.PREFERRED_SIZE, 76, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel28))
            .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel34Layout.createSequentialGroup()
                    .addGap(20, 20, 20)
                    .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(editFamily)
                        .addComponent(deleteFamily)))
                .addGroup(jPanel34Layout.createSequentialGroup()
                    .addGap(18, 18, 18)
                    .addGroup(jPanel34Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(cancelEditFamily)
                        .addComponent(submitEditFamily))))
            .addGap(13, 13, 13))
    );

    familySubTabbedPane.addTab("Informações", jPanel34);

    jPanel35.setBackground(new java.awt.Color(255, 255, 255));

    addApplication.setBackground(new java.awt.Color(22, 113, 204));
    addApplication.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    addApplication.setForeground(new java.awt.Color(255, 255, 255));
    addApplication.setText("Adicionar");
    addApplication.setBorder(null);
    addApplication.setContentAreaFilled(false);
    addApplication.setOpaque(true);
    addApplication.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addApplicationActionPerformed(evt);
        }
    });

    jLabel23.setText("Código:");

    jLabel24.setText("Aprovada:");

    jLabel25.setText("Terreno:");

    jLabel26.setText("Data de Candidatura:");

    jLabel27.setText("Data de Aprovação:");

    previousApplication.setBackground(new java.awt.Color(22, 113, 204));
    previousApplication.setFont(new java.awt.Font("Noto Sans", 0, 14)); // NOI18N
    previousApplication.setForeground(new java.awt.Color(254, 254, 254));
    previousApplication.setText("<");
    previousApplication.setBorderPainted(false);
    previousApplication.setContentAreaFilled(false);
    previousApplication.setEnabled(false);
    previousApplication.setOpaque(true);
    previousApplication.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            previousApplicationActionPerformed(evt);
        }
    });

    nextApplication.setBackground(new java.awt.Color(22, 113, 204));
    nextApplication.setFont(new java.awt.Font("Noto Sans", 0, 14)); // NOI18N
    nextApplication.setForeground(new java.awt.Color(254, 254, 254));
    nextApplication.setText(">");
    nextApplication.setBorderPainted(false);
    nextApplication.setContentAreaFilled(false);
    nextApplication.setEnabled(false);
    nextApplication.setOpaque(true);
    nextApplication.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            nextApplicationActionPerformed(evt);
        }
    });

    deleteApplication.setBackground(new java.awt.Color(22, 113, 204));
    deleteApplication.setForeground(new java.awt.Color(255, 255, 255));
    deleteApplication.setText("Remover");
    deleteApplication.setBorderPainted(false);
    deleteApplication.setContentAreaFilled(false);
    deleteApplication.setOpaque(true);

    editApplication.setBackground(new java.awt.Color(22, 113, 204));
    editApplication.setForeground(new java.awt.Color(255, 255, 255));
    editApplication.setText("Editar");
    editApplication.setBorderPainted(false);
    editApplication.setContentAreaFilled(false);
    editApplication.setOpaque(true);
    editApplication.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            editApplicationActionPerformed(evt);
        }
    });

    jLabel5.setText("Prioridade:");

    jLabel21.setText("Observações:");

    jLabel29.setText("Questionário");

    applicationQuestionnaire.setAutoCreateRowSorter(true);
    applicationQuestionnaire.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Pergunta", "Resposta"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.String.class, java.lang.String.class
        };
        boolean[] canEdit = new boolean [] {
            false, true
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }

        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
        }
    });
    applicationQuestionnaire.setEnabled(false);
    applicationQuestionnaire.getTableHeader().setReorderingAllowed(false);
    jScrollPane7.setViewportView(applicationQuestionnaire);

    applicationDate.setEditable(false);
    applicationDate.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.DateFormatter(new java.text.SimpleDateFormat("dd/MM/yyyy"))));
    applicationDate.setToolTipText("dd/MM/aaaa");

    applicationLocation.setEditable(false);

    applicationApprovalDate.setEditable(false);
    applicationApprovalDate.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.DateFormatter(new java.text.SimpleDateFormat("dd/MM/yyyy"))));
    applicationApprovalDate.setToolTipText("dd/MM/aaaa");

    applicationId.setEditable(false);

    applicationNotes.setEditable(false);
    applicationNotes.setColumns(20);
    applicationNotes.setLineWrap(true);
    applicationNotes.setRows(5);
    applicationNotes.setToolTipText("");
    applicationNotes.setWrapStyleWord(true);
    jScrollPane9.setViewportView(applicationNotes);
    familyNotes.setDocument(new JTextAreaLimit(500));

    editQuestionnaireSubmit.setBackground(new java.awt.Color(22, 113, 204));
    editQuestionnaireSubmit.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    editQuestionnaireSubmit.setForeground(new java.awt.Color(255, 255, 255));
    editQuestionnaireSubmit.setText("Submeter");
    editQuestionnaireSubmit.setBorderPainted(false);
    editQuestionnaireSubmit.setContentAreaFilled(false);
    editQuestionnaireSubmit.setOpaque(true);
    editQuestionnaireSubmit.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            editQuestionnaireSubmitActionPerformed(evt);
        }
    });
    editQuestionnaireSubmit.setVisible(false);

    applicationPriority.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Baixa", "Normal", "Alta" }));
    applicationPriority.setSelectedIndex(-1);
    applicationPriority.setEnabled(false);

    applicationApproved.setBorder(null);
    applicationApproved.setEnabled(false);

    editQuestionnaireCancel.setBackground(new java.awt.Color(22, 113, 204));
    editQuestionnaireCancel.setForeground(new java.awt.Color(255, 255, 255));
    editQuestionnaireCancel.setText("Cancelar");
    editQuestionnaireCancel.setBorderPainted(false);
    editQuestionnaireCancel.setContentAreaFilled(false);
    editQuestionnaireCancel.setOpaque(true);
    editQuestionnaireCancel.setVisible(false);
    editQuestionnaireCancel.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            editQuestionnaireCancelActionPerformed(evt);
        }
    });

    javax.swing.GroupLayout jPanel35Layout = new javax.swing.GroupLayout(jPanel35);
    jPanel35.setLayout(jPanel35Layout);
    jPanel35Layout.setHorizontalGroup(
        jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel35Layout.createSequentialGroup()
            .addGap(55, 55, 55)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jScrollPane7, javax.swing.GroupLayout.DEFAULT_SIZE, 522, Short.MAX_VALUE)
                .addGroup(jPanel35Layout.createSequentialGroup()
                    .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jLabel29)
                        .addGroup(jPanel35Layout.createSequentialGroup()
                            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                .addComponent(jLabel21)
                                .addComponent(jLabel5)
                                .addComponent(jLabel27)
                                .addComponent(jLabel25)
                                .addComponent(jLabel24)
                                .addComponent(jLabel23)
                                .addComponent(jLabel26))
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(applicationDate, javax.swing.GroupLayout.DEFAULT_SIZE, 300, Short.MAX_VALUE)
                                    .addComponent(applicationLocation)
                                    .addComponent(applicationApprovalDate, javax.swing.GroupLayout.PREFERRED_SIZE, 87, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(applicationId, javax.swing.GroupLayout.PREFERRED_SIZE, 87, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jScrollPane9, javax.swing.GroupLayout.DEFAULT_SIZE, 300, Short.MAX_VALUE)
                                    .addComponent(applicationPriority, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addComponent(applicationApproved))))
                    .addGap(0, 0, Short.MAX_VALUE))
                .addGroup(jPanel35Layout.createSequentialGroup()
                    .addComponent(addApplication, javax.swing.GroupLayout.PREFERRED_SIZE, 143, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(editApplication)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(deleteApplication, javax.swing.GroupLayout.PREFERRED_SIZE, 103, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGroup(jPanel35Layout.createSequentialGroup()
                    .addComponent(editQuestionnaireCancel)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(editQuestionnaireSubmit)))
            .addGap(50, 50, 50))
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel35Layout.createSequentialGroup()
            .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel35Layout.createSequentialGroup()
                    .addComponent(applicationPages)
                    .addGap(85, 85, 85))
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel35Layout.createSequentialGroup()
                    .addComponent(previousApplication, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(nextApplication, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGap(40, 40, 40))))
    );
    jPanel35Layout.setVerticalGroup(
        jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel35Layout.createSequentialGroup()
            .addGap(29, 29, 29)
            .addComponent(applicationPages)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(previousApplication)
                .addComponent(nextApplication))
            .addGap(1, 1, 1)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel26)
                .addComponent(applicationDate, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                .addComponent(applicationApproved)
                .addGroup(jPanel35Layout.createSequentialGroup()
                    .addComponent(jLabel24, javax.swing.GroupLayout.PREFERRED_SIZE, 14, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGap(1, 1, 1)))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel5)
                .addComponent(applicationPriority, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel25)
                .addComponent(applicationLocation, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel27)
                .addComponent(applicationApprovalDate, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel23)
                .addComponent(applicationId, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jLabel21)
                .addComponent(jScrollPane9, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(jLabel29)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(jScrollPane7, javax.swing.GroupLayout.PREFERRED_SIZE, 222, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(editQuestionnaireSubmit)
                .addComponent(editQuestionnaireCancel))
            .addGap(18, 18, 18)
            .addGroup(jPanel35Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(addApplication, javax.swing.GroupLayout.PREFERRED_SIZE, 29, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(deleteApplication)
                .addComponent(editApplication))
            .addGap(103, 103, 103))
    );

    jPanel35Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {addApplication, deleteApplication, editApplication});

    familySubTabbedPane.addTab("Candidaturas", jPanel35);

    jPanel36.setBackground(new java.awt.Color(255, 255, 255));

    submitMembers.setBackground(new java.awt.Color(22, 113, 204));
    submitMembers.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    submitMembers.setForeground(new java.awt.Color(255, 255, 255));
    submitMembers.setText("Submeter");
    submitMembers.setBorderPainted(false);
    submitMembers.setContentAreaFilled(false);
    submitMembers.setOpaque(true);
    submitMembers.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            submitMembersActionPerformed(evt);
        }
    });

    removeMember.setBackground(new java.awt.Color(22, 113, 204));
    removeMember.setFont(new java.awt.Font("Noto Sans", 0, 14)); // NOI18N
    removeMember.setForeground(new java.awt.Color(254, 254, 254));
    removeMember.setText("Remover");
    removeMember.setBorderPainted(false);
    removeMember.setContentAreaFilled(false);
    removeMember.setOpaque(true);
    removeMember.setBackground(new java.awt.Color(0, 103, 165));
    removeMember.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    removeMember.setForeground(new java.awt.Color(255, 255, 255));
    removeMember.setBorderPainted(false);
    removeMember.setContentAreaFilled(false);
    removeMember.setOpaque(true);
    removeMember.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            removeMemberActionPerformed(evt);
        }
    });

    addMember.setBackground(new java.awt.Color(22, 113, 204));
    addMember.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    addMember.setForeground(new java.awt.Color(255, 255, 255));
    addMember.setText("Adicionar");
    addMember.setBorderPainted(false);
    addMember.setContentAreaFilled(false);
    addMember.setOpaque(true);
    addMember.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addMemberActionPerformed(evt);
        }
    });

    memberList.setAutoCreateRowSorter(true);
    memberList.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Número", "Nome", "Data de Nascimento", "Grau de Parentesco"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.Integer.class, java.lang.String.class, java.lang.String.class, java.lang.String.class
        };
        boolean[] canEdit = new boolean [] {
            false, true, true, true
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }

        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
        }
    });
    memberList.getTableHeader().setReorderingAllowed(false);
    jScrollPane18.setViewportView(memberList);

    javax.swing.GroupLayout jPanel36Layout = new javax.swing.GroupLayout(jPanel36);
    jPanel36.setLayout(jPanel36Layout);
    jPanel36Layout.setHorizontalGroup(
        jPanel36Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel36Layout.createSequentialGroup()
            .addContainerGap()
            .addGroup(jPanel36Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jScrollPane18, javax.swing.GroupLayout.DEFAULT_SIZE, 603, Short.MAX_VALUE)
                .addGroup(jPanel36Layout.createSequentialGroup()
                    .addComponent(addMember)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(submitMembers)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(removeMember)))
            .addContainerGap())
    );
    jPanel36Layout.setVerticalGroup(
        jPanel36Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel36Layout.createSequentialGroup()
            .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(jScrollPane18, javax.swing.GroupLayout.PREFERRED_SIZE, 378, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addGap(40, 40, 40)
            .addGroup(jPanel36Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(submitMembers)
                .addComponent(removeMember)
                .addComponent(addMember))
            .addGap(238, 238, 238))
    );

    familySubTabbedPane.addTab("Membros", jPanel36);

    addFamily.setBackground(new java.awt.Color(22, 113, 204));
    addFamily.setFont(new java.awt.Font("Lucida Grande", 0, 18)); // NOI18N
    addFamily.setForeground(new java.awt.Color(255, 255, 255));
    addFamily.setText("Adicionar Familia");
    addFamily.setBorderPainted(false);
    addFamily.setContentAreaFilled(false);
    addFamily.setOpaque(true);
    addFamily.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addFamilyActionPerformed(evt);
        }
    });

    familyList.setAutoCreateRowSorter(true);
    familyList.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Código", "Nome", "Representante", "Morada"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.Integer.class, java.lang.String.class, java.lang.String.class, java.lang.String.class
        };
        boolean[] canEdit = new boolean [] {
            false, false, false, false
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }

        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
        }
    });
    familyList.getTableHeader().setReorderingAllowed(false);
    jScrollPane6.setViewportView(familyList);

    javax.swing.GroupLayout jPanel33Layout = new javax.swing.GroupLayout(jPanel33);
    jPanel33.setLayout(jPanel33Layout);
    jPanel33Layout.setHorizontalGroup(
        jPanel33Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel33Layout.createSequentialGroup()
            .addComponent(familySubTabbedPane, javax.swing.GroupLayout.PREFERRED_SIZE, 742, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addGroup(jPanel33Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel33Layout.createSequentialGroup()
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jScrollPane6, javax.swing.GroupLayout.DEFAULT_SIZE, 459, Short.MAX_VALUE)
                    .addContainerGap())
                .addGroup(jPanel33Layout.createSequentialGroup()
                    .addGap(155, 155, 155)
                    .addComponent(addFamily)
                    .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
    );
    jPanel33Layout.setVerticalGroup(
        jPanel33Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel33Layout.createSequentialGroup()
            .addGroup(jPanel33Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel33Layout.createSequentialGroup()
                    .addComponent(familySubTabbedPane, javax.swing.GroupLayout.PREFERRED_SIZE, 736, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGap(0, 0, Short.MAX_VALUE))
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel33Layout.createSequentialGroup()
                    .addGap(0, 0, Short.MAX_VALUE)
                    .addComponent(addFamily)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jScrollPane6, javax.swing.GroupLayout.PREFERRED_SIZE, 688, javax.swing.GroupLayout.PREFERRED_SIZE)))
            .addContainerGap())
    );

    javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
    jPanel1.setLayout(jPanel1Layout);
    jPanel1Layout.setHorizontalGroup(
        jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addComponent(jPanel33, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
    );
    jPanel1Layout.setVerticalGroup(
        jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel1Layout.createSequentialGroup()
            .addComponent(jPanel33, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addGap(694, 694, 694))
    );

    jTabbedPane1.addTab("Famílias", jPanel1);

    jTabbedPane15.setTabPlacement(javax.swing.JTabbedPane.LEFT);
    jTabbedPane15.setFont(new java.awt.Font("Calibri Light", 0, 14)); // NOI18N

    jPanel46.setBackground(new java.awt.Color(255, 255, 255));

    jLabel121.setText("Nome de Projeto:");

    jLabel122.setText("Data Inicio:");

    jLabel123.setText("Data Final:");

    jLabel124.setText("Observações:");

    projectNotes.setColumns(20);
    projectNotes.setLineWrap(true);
    projectNotes.setRows(5);
    projectNotes.setWrapStyleWord(true);
    jScrollPane13.setViewportView(projectNotes);

    jLabel17.setText("Data Entrega Chaves:");

    jLabel35.setText("Data Assinatura Contrato:");

    jLabel36.setText("Orçamento:");

    jLabel37.setText("Custo Final:");

    jLabel3.setText("Data Final Prevista:");

    editProject.setBackground(new java.awt.Color(22, 113, 204));
    editProject.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    editProject.setForeground(new java.awt.Color(255, 255, 255));
    editProject.setText("Editar");
    editProject.setBorderPainted(false);
    editProject.setContentAreaFilled(false);
    editProject.setOpaque(true);
    editProject.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            editProjectActionPerformed(evt);
        }
    });

    deleteProject.setBackground(new java.awt.Color(22, 113, 204));
    deleteProject.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    deleteProject.setForeground(new java.awt.Color(255, 255, 255));
    deleteProject.setText("Remover");
    deleteProject.setBorderPainted(false);
    deleteProject.setContentAreaFilled(false);
    deleteProject.setOpaque(true);

    jLabel33.setText("Próxima Prestação:");

    jtfPDEC.setEditable(false);
    jtfPDEC.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.DateFormatter(new java.text.SimpleDateFormat("dd/MM/yyyy"))));
    jtfPDEC.setToolTipText("dd/MM/aaaa");

    projectSignDate.setEditable(false);
    projectSignDate.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.DateFormatter(new java.text.SimpleDateFormat("dd/MM/yyyy"))));
    projectSignDate.setToolTipText("dd/MM/aaaa");

    jtfPDF.setEditable(false);
    jtfPDF.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.DateFormatter(new java.text.SimpleDateFormat("dd/MM/yyyy"))));
    jtfPDF.setToolTipText("dd/MM/aaaa");

    projectEta.setEditable(false);
    projectEta.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.DateFormatter(new java.text.SimpleDateFormat("dd/MM/yyyy"))));
    projectEta.setToolTipText("dd/MM/aaaa");

    projectStartDate.setEditable(false);
    projectStartDate.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.DateFormatter(new java.text.SimpleDateFormat("dd/MM/yyyy"))));
    projectStartDate.setToolTipText("dd/MM/aaaa");

    cancelEditProject.setBackground(new java.awt.Color(22, 113, 204));
    cancelEditProject.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    cancelEditProject.setForeground(new java.awt.Color(255, 255, 255));
    cancelEditProject.setText("Cancelar");
    cancelEditProject.setBorderPainted(false);
    cancelEditProject.setContentAreaFilled(false);
    cancelEditProject.setOpaque(true);
    cancelEditProject.setVisible(false);
    cancelEditProject.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            cancelEditProjectActionPerformed(evt);
        }
    });

    submitEditProject.setBackground(new java.awt.Color(22, 113, 204));
    submitEditProject.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    submitEditProject.setForeground(new java.awt.Color(255, 255, 255));
    submitEditProject.setText("Submeter");
    submitEditProject.setBorderPainted(false);
    submitEditProject.setContentAreaFilled(false);
    submitEditProject.setOpaque(true);
    submitEditProject.setVisible(false);

    projectBudget.setEditable(false);
    projectBudget.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00; #0.00"))));

    projectFinalCost.setEditable(false);
    projectFinalCost.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00; #0.00"))));

    jtfPVP.setEditable(false);
    jtfPVP.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00; #0.00"))));

    jLabel46.setText("€");

    jLabel47.setText("€");

    jLabel48.setText("€");

    projectName.setEditable(false);
    projectName.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            projectNameActionPerformed(evt);
        }
    });

    javax.swing.GroupLayout jPanel46Layout = new javax.swing.GroupLayout(jPanel46);
    jPanel46.setLayout(jPanel46Layout);
    jPanel46Layout.setHorizontalGroup(
        jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel46Layout.createSequentialGroup()
            .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                .addComponent(jLabel124)
                .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel46Layout.createSequentialGroup()
                        .addGap(61, 61, 61)
                        .addComponent(jLabel121))
                    .addComponent(jLabel3, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel123, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel17, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel122, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel35, javax.swing.GroupLayout.Alignment.TRAILING)))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel46Layout.createSequentialGroup()
                    .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                        .addComponent(projectSignDate, javax.swing.GroupLayout.DEFAULT_SIZE, 105, Short.MAX_VALUE)
                        .addComponent(jtfPDEC, javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(projectStartDate, javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(projectEta, javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jtfPDF, javax.swing.GroupLayout.Alignment.LEADING))
                    .addGap(18, 18, Short.MAX_VALUE)
                    .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                        .addComponent(jLabel33)
                        .addComponent(jLabel37)
                        .addComponent(jLabel36))
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel46Layout.createSequentialGroup()
                            .addComponent(projectFinalCost, javax.swing.GroupLayout.PREFERRED_SIZE, 59, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jLabel47, javax.swing.GroupLayout.PREFERRED_SIZE, 10, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGroup(jPanel46Layout.createSequentialGroup()
                            .addComponent(jtfPVP, javax.swing.GroupLayout.PREFERRED_SIZE, 59, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jLabel48, javax.swing.GroupLayout.PREFERRED_SIZE, 10, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGroup(jPanel46Layout.createSequentialGroup()
                            .addComponent(projectBudget, javax.swing.GroupLayout.PREFERRED_SIZE, 59, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jLabel46, javax.swing.GroupLayout.PREFERRED_SIZE, 10, javax.swing.GroupLayout.PREFERRED_SIZE)))
                    .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addGroup(jPanel46Layout.createSequentialGroup()
                    .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                        .addGroup(jPanel46Layout.createSequentialGroup()
                            .addComponent(cancelEditProject)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(submitEditProject)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(editProject)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(deleteProject))
                        .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(projectName, javax.swing.GroupLayout.PREFERRED_SIZE, 369, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jScrollPane13, javax.swing.GroupLayout.PREFERRED_SIZE, 369, javax.swing.GroupLayout.PREFERRED_SIZE)))
                    .addGap(0, 93, Short.MAX_VALUE))))
    );
    jPanel46Layout.setVerticalGroup(
        jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel46Layout.createSequentialGroup()
            .addGap(20, 20, 20)
            .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel121)
                .addComponent(projectName, javax.swing.GroupLayout.PREFERRED_SIZE, 20, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel37)
                    .addComponent(projectFinalCost, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel47))
                .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel122)
                    .addComponent(projectStartDate, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
            .addGap(4, 4, 4)
            .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel3)
                .addComponent(projectEta, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel33)
                .addComponent(jtfPVP, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel48))
            .addGap(1, 1, 1)
            .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel123)
                .addComponent(jtfPDF, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel36, javax.swing.GroupLayout.PREFERRED_SIZE, 17, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(projectBudget, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel46))
            .addGap(4, 4, 4)
            .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jtfPDEC, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGroup(jPanel46Layout.createSequentialGroup()
                    .addComponent(jLabel17)
                    .addGap(18, 18, 18)
                    .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel35)
                        .addComponent(projectSignDate, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))))
            .addGap(18, 18, 18)
            .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jLabel124)
                .addComponent(jScrollPane13, javax.swing.GroupLayout.PREFERRED_SIZE, 120, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGap(29, 29, 29)
            .addGroup(jPanel46Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(cancelEditProject)
                .addComponent(submitEditProject)
                .addComponent(editProject)
                .addComponent(deleteProject))
            .addGap(351, 351, 351))
    );

    jTabbedPane15.addTab("Informações", jPanel46);

    jPanel10.setBackground(new java.awt.Color(255, 255, 255));

    jLabel32.setText("Observações:");

    paymentPlanNotes.setColumns(20);
    paymentPlanNotes.setRows(5);
    jScrollPane11.setViewportView(paymentPlanNotes);

    addPayment.setBackground(new java.awt.Color(22, 113, 204));
    addPayment.setFont(new java.awt.Font("Noto Sans", 0, 14)); // NOI18N
    addPayment.setForeground(new java.awt.Color(254, 254, 254));
    addPayment.setText("+");
    addPayment.setBorderPainted(false);
    addPayment.setContentAreaFilled(false);
    addPayment.setOpaque(true);
    addPayment.setPreferredSize(new java.awt.Dimension(22, 40));
    addPayment.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addPaymentActionPerformed(evt);
        }
    });

    deletePayment.setBackground(new java.awt.Color(22, 113, 204));
    deletePayment.setFont(new java.awt.Font("Noto Sans", 0, 14)); // NOI18N
    deletePayment.setForeground(new java.awt.Color(254, 254, 254));
    deletePayment.setText("-");
    deletePayment.setBorderPainted(false);
    deletePayment.setContentAreaFilled(false);
    deletePayment.setOpaque(true);
    deletePayment.setPreferredSize(new java.awt.Dimension(22, 40));
    deletePayment.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            deletePaymentActionPerformed(evt);
        }
    });

    editPaymentPlan.setBackground(new java.awt.Color(22, 113, 204));
    editPaymentPlan.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    editPaymentPlan.setForeground(new java.awt.Color(255, 255, 255));
    editPaymentPlan.setText("Editar");
    editPaymentPlan.setBorderPainted(false);
    editPaymentPlan.setContentAreaFilled(false);
    editPaymentPlan.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));
    editPaymentPlan.setOpaque(true);

    paymentPlan.setAutoCreateRowSorter(true);
    paymentPlan.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Valor", "Data Limite", "Paga"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.String.class, java.lang.String.class, java.lang.Object.class
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }
    });
    paymentPlan.getTableHeader().setReorderingAllowed(false);
    jScrollPane12.setViewportView(paymentPlan);

    jLabel31.setFont(new java.awt.Font("Calibri Light", 0, 24)); // NOI18N
    jLabel31.setText("Plano de Pagamentos");

    javax.swing.GroupLayout jPanel10Layout = new javax.swing.GroupLayout(jPanel10);
    jPanel10.setLayout(jPanel10Layout);
    jPanel10Layout.setHorizontalGroup(
        jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel10Layout.createSequentialGroup()
            .addGroup(jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel10Layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jScrollPane12, javax.swing.GroupLayout.DEFAULT_SIZE, 608, Short.MAX_VALUE))
                .addGroup(jPanel10Layout.createSequentialGroup()
                    .addGap(21, 21, 21)
                    .addComponent(jLabel31)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(addPayment, javax.swing.GroupLayout.PREFERRED_SIZE, 37, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(deletePayment, javax.swing.GroupLayout.PREFERRED_SIZE, 37, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGroup(jPanel10Layout.createSequentialGroup()
                    .addGap(45, 45, 45)
                    .addComponent(jLabel32)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jScrollPane11, javax.swing.GroupLayout.PREFERRED_SIZE, 436, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGap(0, 51, Short.MAX_VALUE)))
            .addContainerGap())
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel10Layout.createSequentialGroup()
            .addGap(0, 0, Short.MAX_VALUE)
            .addComponent(editPaymentPlan)
            .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    jPanel10Layout.setVerticalGroup(
        jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel10Layout.createSequentialGroup()
            .addGap(17, 17, 17)
            .addGroup(jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                .addComponent(jLabel31)
                .addGroup(jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(deletePayment, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE)
                    .addComponent(addPayment, javax.swing.GroupLayout.PREFERRED_SIZE, 24, javax.swing.GroupLayout.PREFERRED_SIZE)))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(jScrollPane12, javax.swing.GroupLayout.PREFERRED_SIZE, 251, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(editPaymentPlan)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel10Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jLabel32)
                .addComponent(jScrollPane11, javax.swing.GroupLayout.PREFERRED_SIZE, 115, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGap(252, 252, 252))
    );

    jTabbedPane15.addTab("Prestações", jPanel10);

    jPanel6.setBackground(new java.awt.Color(255, 255, 255));

    addTask.setText("+");
    addTask.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addTaskActionPerformed(evt);
        }
    });

    removeTask.setText("-");
    removeTask.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            removeTaskActionPerformed(evt);
        }
    });

    taskList.setAutoCreateRowSorter(true);
    taskList.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Nome", "Data Início", "Estado"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.String.class, java.lang.String.class, java.lang.String.class
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }
    });
    taskList.getTableHeader().setReorderingAllowed(false);
    jScrollPane22.setViewportView(taskList);

    taskViewDetails.setBackground(new java.awt.Color(22, 113, 204));
    taskViewDetails.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    taskViewDetails.setForeground(new java.awt.Color(255, 255, 255));
    taskViewDetails.setText("Detalhes");
    taskViewDetails.setBorderPainted(false);
    taskViewDetails.setContentAreaFilled(false);
    taskViewDetails.setOpaque(true);

    javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
    jPanel6.setLayout(jPanel6Layout);
    jPanel6Layout.setHorizontalGroup(
        jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel6Layout.createSequentialGroup()
            .addContainerGap()
            .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jScrollPane22, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 631, Short.MAX_VALUE)
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
                    .addGap(0, 0, Short.MAX_VALUE)
                    .addComponent(addTask, javax.swing.GroupLayout.PREFERRED_SIZE, 42, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(removeTask, javax.swing.GroupLayout.PREFERRED_SIZE, 43, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGroup(jPanel6Layout.createSequentialGroup()
                    .addGap(0, 0, Short.MAX_VALUE)
                    .addComponent(taskViewDetails)
                    .addGap(0, 0, Short.MAX_VALUE)))
            .addContainerGap())
    );
    jPanel6Layout.setVerticalGroup(
        jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel6Layout.createSequentialGroup()
            .addGap(36, 36, 36)
            .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(addTask)
                .addComponent(removeTask))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(jScrollPane22, javax.swing.GroupLayout.DEFAULT_SIZE, 612, Short.MAX_VALUE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(taskViewDetails)
            .addContainerGap())
    );

    jTabbedPane15.addTab("Tarefas", jPanel6);

    addProject.setBackground(new java.awt.Color(22, 113, 204));
    addProject.setFont(new java.awt.Font("Lucida Grande", 0, 18)); // NOI18N
    addProject.setForeground(new java.awt.Color(255, 255, 255));
    addProject.setText("Adicionar projeto");
    addProject.setBorderPainted(false);
    addProject.setContentAreaFilled(false);
    addProject.setOpaque(true);
    addProject.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addProjectActionPerformed(evt);
        }
    });

    projectList.setAutoCreateRowSorter(true);
    projectList.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {
            {null, null, null},
            {null, null, null},
            {null, null, null}
        },
        new String [] {
            "Código", "Nome", "Orçamento"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.Integer.class, java.lang.String.class, java.lang.String.class
        };
        boolean[] canEdit = new boolean [] {
            false, false, false
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }

        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
        }
    });
    projectList.getTableHeader().setReorderingAllowed(false);
    jScrollPane16.setViewportView(projectList);

    javax.swing.GroupLayout jPanel45Layout = new javax.swing.GroupLayout(jPanel45);
    jPanel45.setLayout(jPanel45Layout);
    jPanel45Layout.setHorizontalGroup(
        jPanel45Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel45Layout.createSequentialGroup()
            .addComponent(jTabbedPane15, javax.swing.GroupLayout.PREFERRED_SIZE, 741, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addGroup(jPanel45Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel45Layout.createSequentialGroup()
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jScrollPane16, javax.swing.GroupLayout.DEFAULT_SIZE, 441, Short.MAX_VALUE)
                    .addContainerGap())
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel45Layout.createSequentialGroup()
                    .addGap(157, 157, 157)
                    .addComponent(addProject)
                    .addContainerGap(157, Short.MAX_VALUE))))
    );
    jPanel45Layout.setVerticalGroup(
        jPanel45Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel45Layout.createSequentialGroup()
            .addGroup(jPanel45Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel45Layout.createSequentialGroup()
                    .addGap(11, 11, 11)
                    .addComponent(addProject)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jScrollPane16, javax.swing.GroupLayout.PREFERRED_SIZE, 688, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addComponent(jTabbedPane15, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE))
            .addContainerGap())
    );

    javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
    jPanel2.setLayout(jPanel2Layout);
    jPanel2Layout.setHorizontalGroup(
        jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGap(0, 1219, Short.MAX_VALUE)
        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel45, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    jPanel2Layout.setVerticalGroup(
        jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGap(0, 757, Short.MAX_VALUE)
        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addComponent(jPanel45, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap()))
    );

    jTabbedPane1.addTab("Projetos", jPanel2);

    jTabbedPane12.setBackground(new java.awt.Color(255, 255, 255));
    jTabbedPane12.setTabPlacement(javax.swing.JTabbedPane.LEFT);
    jTabbedPane12.setFont(new java.awt.Font("Calibri Light", 0, 14)); // NOI18N

    jPanel25.setBackground(new java.awt.Color(255, 255, 255));

    jLabel110.setText("Nome:");

    jLabel111.setText("Morada:");

    jLabel112.setText("Equipa:");

    jLabel114.setText("Observações:");

    volunteerNotes.setEditable(false);
    volunteerNotes.setColumns(20);
    volunteerNotes.setLineWrap(true);
    volunteerNotes.setRows(5);
    volunteerNotes.setWrapStyleWord(true);
    jScrollPane8.setViewportView(volunteerNotes);

    jLabel9.setText("Data Nasc:");

    jLabel10.setText("Escolaridade:");

    jLabel11.setText("Estado Civil:");

    jLabel12.setText("NIF:");

    jLabel13.setText("NIB:");

    jLabel14.setText("Naturalidade:");

    jLabel15.setText("Nacionalidade:");

    editVolunteer.setBackground(new java.awt.Color(22, 113, 204));
    editVolunteer.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    editVolunteer.setForeground(new java.awt.Color(255, 255, 255));
    editVolunteer.setText("Editar");
    editVolunteer.setBorderPainted(false);
    editVolunteer.setContentAreaFilled(false);
    editVolunteer.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));
    editVolunteer.setOpaque(true);
    editVolunteer.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            editVolunteerActionPerformed(evt);
        }
    });

    deleteVolunteer.setBackground(new java.awt.Color(22, 113, 204));
    deleteVolunteer.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    deleteVolunteer.setForeground(new java.awt.Color(255, 255, 255));
    deleteVolunteer.setText("Remover");
    deleteVolunteer.setBorderPainted(false);
    deleteVolunteer.setContentAreaFilled(false);
    deleteVolunteer.setOpaque(true);

    volunteerName.setEditable(false);

    volunteerAddress.setEditable(false);

    volunteerTeam.setModel(new javax.swing.DefaultComboBoxModel(new String[] { " " }));
    volunteerTeam.setEnabled(false);

    addVolunteerTeam.setBackground(new java.awt.Color(22, 113, 204));
    addVolunteerTeam.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    addVolunteerTeam.setForeground(new java.awt.Color(255, 255, 255));
    addVolunteerTeam.setText("Nova Equipa");
    addVolunteerTeam.setBorderPainted(false);
    addVolunteerTeam.setContentAreaFilled(false);
    addVolunteerTeam.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));
    addVolunteerTeam.setOpaque(true);
    addVolunteerTeam.setVisible(false);

    volunteerBirthDate.setEditable(false);
    volunteerBirthDate.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.DateFormatter(new java.text.SimpleDateFormat("dd/MM/yyyy"))));
    volunteerBirthDate.setToolTipText("dd/MM/aaaa");
    volunteerBirthDate.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            volunteerBirthDateActionPerformed(evt);
        }
    });

    volunteerEducation.setEditable(false);

    volunteerBirthPlace.setEditable(false);

    volunteerMaritalStatus.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Solteiro(a)", "Casado(a)", "Divorciado(a)", "Viúvo(a)" }));
    volunteerMaritalStatus.setEnabled(false);

    volunteerNationality.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Afeganistão", "África do Sul", "Albânia", "Alemanha", "Andorra", "Angola", "Arábia Saudita", "Argélia", "Argentina", "Armênia", "Austrália", "Áustria", "Azerbaijão", "Bahamas", "Bahrein", "Bangladesh", "Barbados", "Bélgica", "Belize", "Benin", "Bielorrússia", "Bolívia", "Bósnia", "Botsuana", "Brasil", "Brunei", "Bulgária", "Burkina-Fasso", "Burundi", "Butão", "Cabo Verde", "Camarões", "Camboja", "Canadá", "Catar", "Cazaquistão", "Chade", "Chile", "China", "Chipre", "Cingapura", "Colômbia", "Comores", "Congo", "Coréia do Norte", "Coréia do Sul", "Costado Marfim", "Costa Rica", "Croácia", "Cuba", "Dinamarca", "Djibuti", "Dominica", "Egito", "El Salvador", "Emirados Árabes Unidos", "Equador", "Eritreia", "Escócia", "Eslováquia", "Eslovênia", "Espanha", "Estados Unidos", "Estônia", "Etiópia", "Federação Russa", "Fiji", "Filipinas", "Finlândia", "Formosa Taiwan", "França", "Gabão", "Gâmbia", "Gana", "Geórgia", "Grã-Bretanha", "Granada", "Grécia", "Groenlândia", "Guatemala", "Guiana", "Guiana Francesa", "Guiné", "Guiné Bissau", "Guiné Equatorial", "Haiti", "Holanda", "Honduras", "Hungria", "Iêmen", "Ilhas Marshall", "Ilhas Salomão", "Índia", "Indonésia", "Irão", "Iraque", "Irlanda", "Irlanda do Norte", "Islândia", "Israel", "Itália", "Jamaica", "Japão", "Jordânia", "Kiribati", "Kuwait", "Laos", "Lesoto", "Letônia", "Líbano", "Libéria", "Líbia", "Liechtenstein", "Lituânia", "Luxemburgo", "Macedônia", "Madagascar", "Malásia", "Malauí", "Maldivas", "Mali", "Malta", "Marrocos", "Maurício", "Mauritânia", "México", "Mianmar", "Micronésia", "Moçambique", "Moldávia", "Mônaco", "Mongólia", "Namíbia", "Nauru", "Nepal", "Nicarágua", "Níger", "Nigéria", "Noruega", "Nova Zelândia", "Omã", "Palau", "Panamá", "Papua Nova Guiné", "Paquistão", "Paraguai", "Peru", "Polônia", "Porto Rico", "Portugal", "Quênia", "Quirguistão", "Reino Unido", "Rep.Centro-Africana", "Rep.Dominicana", "República Tcheca", "Romênia", "Ruanda", "Samoa", "SanMarino", "Santa Lúcia", "São Cristóvão e Névis", "São Tomé e Príncipe", "São Vicente e Granadinas", "Seicheles", "Senegal", "Serra Leoa", "Sérvia e Montenegro", "Síria", "Somália", "Sri-Lanka", "Suazilândia", "Sudão", "Suécia", "Suáça", "Suriname", "Tadjiquistão", "Tailândia", "Tanzânia", "Togo", "Tonga", "Trinidade Tobago", "Tunísia", "Turcomenistão", "Turquia", "Tuvalu", "Ucrânia", "Uganda", "Uruguai", "Uzbequistão", "Vanuatu", "Vaticano", "Venezuela", "Vietnã", "Zaire", "Zâmbia", "Zimbábue " }));
    volunteerNationality.setSelectedIndex(149);
    volunteerNationality.setEnabled(false);
    volunteerNationality.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            volunteerNationalityActionPerformed(evt);
        }
    });

    volunteerNib.setEditable(false);

    volunteerNif.setEditable(false);

    cancelEditVolunteer.setBackground(new java.awt.Color(22, 113, 204));
    cancelEditVolunteer.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    cancelEditVolunteer.setForeground(new java.awt.Color(255, 255, 255));
    cancelEditVolunteer.setText("Cancelar");
    cancelEditVolunteer.setBorderPainted(false);
    cancelEditVolunteer.setContentAreaFilled(false);
    cancelEditVolunteer.setOpaque(true);
    cancelEditVolunteer.setVisible(false);
    cancelEditVolunteer.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            cancelEditVolunteerActionPerformed(evt);
        }
    });

    submitEditVolunteer.setBackground(new java.awt.Color(22, 113, 204));
    submitEditVolunteer.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    submitEditVolunteer.setForeground(new java.awt.Color(255, 255, 255));
    submitEditVolunteer.setText("Submeter");
    submitEditVolunteer.setBorderPainted(false);
    submitEditVolunteer.setContentAreaFilled(false);
    submitEditVolunteer.setOpaque(true);
    submitEditVolunteer.setVisible(false);
    submitEditVolunteer.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            submitEditVolunteerActionPerformed(evt);
        }
    });

    javax.swing.GroupLayout jPanel25Layout = new javax.swing.GroupLayout(jPanel25);
    jPanel25.setLayout(jPanel25Layout);
    jPanel25Layout.setHorizontalGroup(
        jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel25Layout.createSequentialGroup()
            .addGap(34, 34, 34)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                .addComponent(jLabel11)
                .addComponent(jLabel9)
                .addComponent(jLabel10)
                .addComponent(jLabel14)
                .addComponent(jLabel15))
            .addGap(246, 246, 246)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                .addComponent(jLabel13)
                .addComponent(jLabel12))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(volunteerNib, javax.swing.GroupLayout.PREFERRED_SIZE, 208, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(volunteerNif, javax.swing.GroupLayout.PREFERRED_SIZE, 208, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGap(0, 83, Short.MAX_VALUE))
        .addGroup(jPanel25Layout.createSequentialGroup()
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel25Layout.createSequentialGroup()
                    .addGap(38, 38, 38)
                    .addComponent(jLabel114)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel25Layout.createSequentialGroup()
                    .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel25Layout.createSequentialGroup()
                            .addComponent(jLabel111)
                            .addGap(7, 7, 7))
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel25Layout.createSequentialGroup()
                            .addComponent(jLabel112)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED))
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel25Layout.createSequentialGroup()
                            .addComponent(jLabel110)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)))))
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(volunteerAddress, javax.swing.GroupLayout.DEFAULT_SIZE, 555, Short.MAX_VALUE)
                .addComponent(volunteerName)
                .addGroup(jPanel25Layout.createSequentialGroup()
                    .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(volunteerBirthDate, javax.swing.GroupLayout.PREFERRED_SIZE, 376, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(volunteerMaritalStatus, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(volunteerEducation, javax.swing.GroupLayout.PREFERRED_SIZE, 208, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(volunteerBirthPlace, javax.swing.GroupLayout.PREFERRED_SIZE, 208, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(volunteerNationality, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGroup(jPanel25Layout.createSequentialGroup()
                            .addComponent(volunteerTeam, javax.swing.GroupLayout.PREFERRED_SIZE, 95, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGap(25, 25, 25)
                            .addComponent(addVolunteerTeam))
                        .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                            .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanel25Layout.createSequentialGroup()
                                .addComponent(cancelEditVolunteer)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(submitEditVolunteer))
                            .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanel25Layout.createSequentialGroup()
                                .addComponent(editVolunteer)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(deleteVolunteer))
                            .addComponent(jScrollPane8, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.PREFERRED_SIZE, 475, javax.swing.GroupLayout.PREFERRED_SIZE)))
                    .addGap(0, 19, Short.MAX_VALUE)))
            .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    jPanel25Layout.setVerticalGroup(
        jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel25Layout.createSequentialGroup()
            .addGap(20, 20, 20)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel110)
                .addComponent(volunteerName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGap(6, 6, 6)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel111)
                .addComponent(volunteerAddress, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel112)
                .addComponent(volunteerTeam, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(addVolunteerTeam))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel9)
                .addComponent(volunteerBirthDate, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel11)
                .addComponent(volunteerMaritalStatus, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel10)
                .addComponent(volunteerEducation, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel13)
                .addComponent(volunteerNib, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel14)
                .addComponent(volunteerBirthPlace, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel12)
                .addComponent(volunteerNif, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel15)
                .addComponent(volunteerNationality, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jLabel114)
                .addComponent(jScrollPane8, javax.swing.GroupLayout.PREFERRED_SIZE, 122, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(submitEditVolunteer)
                .addComponent(cancelEditVolunteer))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel25Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(editVolunteer)
                .addComponent(deleteVolunteer))
            .addContainerGap(241, Short.MAX_VALUE))
    );

    jTabbedPane12.addTab("Informações", jPanel25);

    jPanel11.setBackground(new java.awt.Color(255, 255, 255));

    volunteerViewEventDetails.setBackground(new java.awt.Color(22, 113, 204));
    volunteerViewEventDetails.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    volunteerViewEventDetails.setForeground(new java.awt.Color(255, 255, 255));
    volunteerViewEventDetails.setText("Detalhes");
    volunteerViewEventDetails.setBorderPainted(false);
    volunteerViewEventDetails.setContentAreaFilled(false);
    volunteerViewEventDetails.setOpaque(true);

    volunteerEventList.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Data", "Local", "Valor Obtido", "Nr. Participantes"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.String.class, java.lang.String.class, java.lang.Double.class, java.lang.Integer.class
        };
        boolean[] canEdit = new boolean [] {
            false, false, false, false
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }

        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
        }
    });
    volunteerEventList.getTableHeader().setReorderingAllowed(false);
    jScrollPane19.setViewportView(volunteerEventList);

    javax.swing.GroupLayout jPanel11Layout = new javax.swing.GroupLayout(jPanel11);
    jPanel11.setLayout(jPanel11Layout);
    jPanel11Layout.setHorizontalGroup(
        jPanel11Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel11Layout.createSequentialGroup()
            .addContainerGap()
            .addGroup(jPanel11Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jScrollPane19)
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel11Layout.createSequentialGroup()
                    .addGap(0, 257, Short.MAX_VALUE)
                    .addComponent(volunteerViewEventDetails)
                    .addGap(0, 289, Short.MAX_VALUE)))
            .addContainerGap())
    );
    jPanel11Layout.setVerticalGroup(
        jPanel11Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel11Layout.createSequentialGroup()
            .addContainerGap()
            .addComponent(jScrollPane19, javax.swing.GroupLayout.DEFAULT_SIZE, 671, Short.MAX_VALUE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(volunteerViewEventDetails)
            .addGap(12, 12, 12))
    );

    jTabbedPane12.addTab("Eventos", jPanel11);

    jPanel12.setBackground(new java.awt.Color(255, 255, 255));

    volunteerViewProjectDetails.setBackground(new java.awt.Color(22, 113, 204));
    volunteerViewProjectDetails.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    volunteerViewProjectDetails.setForeground(new java.awt.Color(255, 255, 255));
    volunteerViewProjectDetails.setText("Detalhes");
    volunteerViewProjectDetails.setBorderPainted(false);
    volunteerViewProjectDetails.setContentAreaFilled(false);
    volunteerViewProjectDetails.setOpaque(true);

    jTable8.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Nome", "Data de Início", "Orçamento/Custo Final", "Terminado"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.String.class, java.lang.String.class, java.lang.Double.class, java.lang.Boolean.class
        };
        boolean[] canEdit = new boolean [] {
            false, false, false, false
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }

        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
        }
    });
    jTable8.getTableHeader().setReorderingAllowed(false);
    jScrollPane20.setViewportView(jTable8);
    if (jTable8.getColumnModel().getColumnCount() > 0) {
        jTable8.getColumnModel().getColumn(3).setCellEditor(new DefaultCellEditor( new JCheckBox() ));
    }

    javax.swing.GroupLayout jPanel12Layout = new javax.swing.GroupLayout(jPanel12);
    jPanel12.setLayout(jPanel12Layout);
    jPanel12Layout.setHorizontalGroup(
        jPanel12Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel12Layout.createSequentialGroup()
            .addContainerGap()
            .addGroup(jPanel12Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jScrollPane20)
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel12Layout.createSequentialGroup()
                    .addGap(0, 255, Short.MAX_VALUE)
                    .addComponent(volunteerViewProjectDetails)
                    .addGap(0, 291, Short.MAX_VALUE)))
            .addContainerGap())
    );
    jPanel12Layout.setVerticalGroup(
        jPanel12Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel12Layout.createSequentialGroup()
            .addContainerGap()
            .addComponent(jScrollPane20, javax.swing.GroupLayout.DEFAULT_SIZE, 671, Short.MAX_VALUE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(volunteerViewProjectDetails)
            .addGap(12, 12, 12))
    );

    jTabbedPane12.addTab("Projetos", jPanel12);

    addVolunteer.setBackground(new java.awt.Color(22, 113, 204));
    addVolunteer.setFont(new java.awt.Font("Lucida Grande", 0, 18)); // NOI18N
    addVolunteer.setForeground(new java.awt.Color(255, 255, 255));
    addVolunteer.setText("Adicionar Voluntário");
    addVolunteer.setBorderPainted(false);
    addVolunteer.setContentAreaFilled(false);
    addVolunteer.setOpaque(true);
    addVolunteer.addMouseListener(new java.awt.event.MouseAdapter() {
        public void mouseClicked(java.awt.event.MouseEvent evt) {
            addVolunteerMouseClicked(evt);
        }
    });
    addVolunteer.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addVolunteerActionPerformed(evt);
        }
    });

    jTable3.setAutoCreateRowSorter(true);
    jTable3.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Código", "Nome", "Equipa"
        }
    ) {
        boolean[] canEdit = new boolean [] {
            false, false, true
        };

        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
        }
    });
    jTable3.getTableHeader().setReorderingAllowed(false);
    jScrollPane10.setViewportView(jTable3);

    javax.swing.GroupLayout jPanel24Layout = new javax.swing.GroupLayout(jPanel24);
    jPanel24.setLayout(jPanel24Layout);
    jPanel24Layout.setHorizontalGroup(
        jPanel24Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel24Layout.createSequentialGroup()
            .addComponent(jTabbedPane12, javax.swing.GroupLayout.PREFERRED_SIZE, 741, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addGroup(jPanel24Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel24Layout.createSequentialGroup()
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jScrollPane10, javax.swing.GroupLayout.DEFAULT_SIZE, 460, Short.MAX_VALUE)
                    .addContainerGap())
                .addGroup(jPanel24Layout.createSequentialGroup()
                    .addGap(142, 142, 142)
                    .addComponent(addVolunteer)
                    .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))))
    );
    jPanel24Layout.setVerticalGroup(
        jPanel24Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel24Layout.createSequentialGroup()
            .addGap(0, 0, 0)
            .addGroup(jPanel24Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                .addGroup(jPanel24Layout.createSequentialGroup()
                    .addGap(0, 0, Short.MAX_VALUE)
                    .addComponent(addVolunteer)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jScrollPane10, javax.swing.GroupLayout.PREFERRED_SIZE, 685, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addComponent(jTabbedPane12))
            .addContainerGap())
    );

    javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
    jPanel3.setLayout(jPanel3Layout);
    jPanel3Layout.setHorizontalGroup(
        jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGap(0, 1219, Short.MAX_VALUE)
        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel24, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    jPanel3Layout.setVerticalGroup(
        jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGap(0, 757, Short.MAX_VALUE)
        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addComponent(jPanel24, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap()))
    );

    jTabbedPane1.addTab("Voluntários", jPanel3);

    jTabbedPane14.setBackground(new java.awt.Color(255, 255, 255));
    jTabbedPane14.setTabPlacement(javax.swing.JTabbedPane.LEFT);
    jTabbedPane14.setFont(new java.awt.Font("Calibri Light", 0, 14)); // NOI18N

    jPanel44.setBackground(new java.awt.Color(255, 255, 255));

    jLabel115.setText("Local:");

    jLabel130.setText("Data:");

    jLabel131.setText("Valor angariado:");

    jLabel133.setText("Observações:");

    eventObservation.setEditable(false);

    eventObservation.setLineWrap(true);

    eventObservation.setToolTipText("");

    eventObservation.setWrapStyleWord(true);
    eventObservation.setColumns(20);
    eventObservation.setRows(5);
    jScrollPane32.setViewportView(eventObservation);
    eventObservation.setDocument(new JTextAreaLimit(500));

    editEventButton.setBackground(new java.awt.Color(22, 113, 204));
    editEventButton.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    editEventButton.setForeground(new java.awt.Color(255, 255, 255));
    editEventButton.setText("Editar");
    editEventButton.setBorderPainted(false);
    editEventButton.setContentAreaFilled(false);
    editEventButton.setOpaque(true);
    editEventButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            editEventButtonActionPerformed(evt);
        }
    });

    removeEventButton.setBackground(new java.awt.Color(22, 113, 204));
    removeEventButton.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    removeEventButton.setForeground(new java.awt.Color(255, 255, 255));
    removeEventButton.setText("Remover");
    removeEventButton.setBorderPainted(false);
    removeEventButton.setContentAreaFilled(false);
    removeEventButton.setOpaque(true);
    removeEventButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            removeEventButtonActionPerformed(evt);
        }
    });

    jLabel1.setText("Nº participantes:");

    eventDate.setEditable(false);
    eventDate.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.DateFormatter(new java.text.SimpleDateFormat("dd/MM/yyyy"))));
    eventDate.setToolTipText("dd/MM/aaaa");
    eventDate.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            eventDateActionPerformed(evt);
        }
    });

    eventAddress.setEditable(false);
    eventAddress.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            eventAddressActionPerformed(evt);
        }
    });

    eventRaisedValue.setEditable(false);
    eventRaisedValue.setFormatterFactory(new javax.swing.text.DefaultFormatterFactory(new javax.swing.text.NumberFormatter(new java.text.DecimalFormat("#0.00; #0.00"))));
    eventRaisedValue.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            eventRaisedValueActionPerformed(evt);
        }
    });

    cancelEditEventButton.setVisible(false);
    cancelEditEventButton.setBackground(new java.awt.Color(22, 113, 204));
    cancelEditEventButton.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    cancelEditEventButton.setForeground(new java.awt.Color(255, 255, 255));
    cancelEditEventButton.setText("Cancelar");
    cancelEditEventButton.setBorderPainted(false);
    cancelEditEventButton.setContentAreaFilled(false);
    cancelEditEventButton.setOpaque(true);
    cancelEditEventButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            cancelEditEventButtonActionPerformed(evt);
        }
    });

    submitEditEventButton.setVisible(false);
    submitEditEventButton.setBackground(new java.awt.Color(22, 113, 204));
    submitEditEventButton.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    submitEditEventButton.setForeground(new java.awt.Color(255, 255, 255));
    submitEditEventButton.setText("Submeter");
    submitEditEventButton.setBorderPainted(false);
    submitEditEventButton.setContentAreaFilled(false);
    submitEditEventButton.setOpaque(true);

    jLabel4.setText("€");

    eventParticipantNmb.setModel(new javax.swing.SpinnerNumberModel(Integer.valueOf(0), Integer.valueOf(0), null, Integer.valueOf(1)));

    javax.swing.GroupLayout jPanel44Layout = new javax.swing.GroupLayout(jPanel44);
    jPanel44.setLayout(jPanel44Layout);
    jPanel44Layout.setHorizontalGroup(
        jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel44Layout.createSequentialGroup()
            .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel44Layout.createSequentialGroup()
                    .addGap(19, 19, 19)
                    .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                        .addComponent(jLabel133)
                        .addComponent(jLabel131)
                        .addComponent(jLabel115)
                        .addComponent(jLabel1))
                    .addGap(5, 5, 5))
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel44Layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jLabel130)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)))
            .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel44Layout.createSequentialGroup()
                    .addComponent(cancelEditEventButton)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(submitEditEventButton)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 200, Short.MAX_VALUE)
                    .addComponent(editEventButton)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(removeEventButton))
                .addComponent(jScrollPane32, javax.swing.GroupLayout.DEFAULT_SIZE, 543, Short.MAX_VALUE)
                .addComponent(eventAddress)
                .addGroup(jPanel44Layout.createSequentialGroup()
                    .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel44Layout.createSequentialGroup()
                            .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                                .addComponent(eventParticipantNmb, javax.swing.GroupLayout.Alignment.LEADING)
                                .addComponent(eventRaisedValue, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 101, Short.MAX_VALUE))
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jLabel4))
                        .addComponent(eventDate, javax.swing.GroupLayout.PREFERRED_SIZE, 101, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGap(0, 0, Short.MAX_VALUE)))
            .addGap(22, 22, 22))
    );
    jPanel44Layout.setVerticalGroup(
        jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel44Layout.createSequentialGroup()
            .addGap(21, 21, 21)
            .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel115)
                .addComponent(eventAddress, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGap(5, 5, 5)
            .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel130)
                .addComponent(eventDate, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel131)
                .addComponent(eventRaisedValue, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel4))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
            .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel1)
                .addComponent(eventParticipantNmb, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGap(12, 12, 12)
            .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jLabel133)
                .addComponent(jScrollPane32, javax.swing.GroupLayout.PREFERRED_SIZE, 101, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(removeEventButton)
                    .addComponent(editEventButton))
                .addGroup(jPanel44Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(cancelEditEventButton)
                    .addComponent(submitEditEventButton)))
            .addContainerGap())
    );

    jTabbedPane14.addTab("Informações", jPanel44);

    jPanel7.setBackground(new java.awt.Color(255, 255, 255));

    participantsTable.setAutoCreateRowSorter(true);
    participantsTable.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Nome", "Contacto"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.String.class, java.lang.String.class
        };
        boolean[] canEdit = new boolean [] {
            false, false
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }

        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
        }
    });
    participantsTable.getTableHeader().setReorderingAllowed(false);
    jScrollPane17.setViewportView(participantsTable);

    editParticipantsButton.setBackground(new java.awt.Color(22, 113, 204));
    editParticipantsButton.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    editParticipantsButton.setForeground(new java.awt.Color(255, 255, 255));
    editParticipantsButton.setText("Editar");
    editParticipantsButton.setBorderPainted(false);
    editParticipantsButton.setContentAreaFilled(false);
    editParticipantsButton.setOpaque(true);
    editParticipantsButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            editParticipantsButtonActionPerformed(evt);
        }
    });

    javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
    jPanel7.setLayout(jPanel7Layout);
    jPanel7Layout.setHorizontalGroup(
        jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel7Layout.createSequentialGroup()
            .addContainerGap(49, Short.MAX_VALUE)
            .addComponent(jScrollPane17, javax.swing.GroupLayout.PREFERRED_SIZE, 521, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addContainerGap(58, Short.MAX_VALUE))
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel7Layout.createSequentialGroup()
            .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(editParticipantsButton)
            .addGap(284, 284, 284))
    );
    jPanel7Layout.setVerticalGroup(
        jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel7Layout.createSequentialGroup()
            .addGap(28, 28, 28)
            .addComponent(jScrollPane17, javax.swing.GroupLayout.PREFERRED_SIZE, 499, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(editParticipantsButton)
            .addContainerGap(168, Short.MAX_VALUE))
    );

    jTabbedPane14.addTab("Participantes", jPanel7);

    addEventButton.setBackground(new java.awt.Color(22, 113, 204));
    addEventButton.setFont(new java.awt.Font("Lucida Grande", 0, 18)); // NOI18N
    addEventButton.setForeground(new java.awt.Color(255, 255, 255));
    addEventButton.setText("Adicionar Evento");
    addEventButton.setBorderPainted(false);
    addEventButton.setContentAreaFilled(false);
    addEventButton.setOpaque(true);
    addEventButton.setRequestFocusEnabled(false);
    addEventButton.addMouseListener(new java.awt.event.MouseAdapter() {
        public void mouseClicked(java.awt.event.MouseEvent evt) {
            addEventButtonMouseClicked(evt);
        }
    });
    addEventButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addEventButtonActionPerformed(evt);
        }
    });

    eventTable.setAutoCreateRowSorter(true);
    eventTable.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Local", "Data", "Nºparticipantes"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.String.class, java.lang.String.class, java.lang.String.class
        };
        boolean[] canEdit = new boolean [] {
            false, false, false
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }

        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
        }
    });
    eventTable.getTableHeader().setReorderingAllowed(false);
    jScrollPane21.setViewportView(eventTable);

    javax.swing.GroupLayout jPanel43Layout = new javax.swing.GroupLayout(jPanel43);
    jPanel43.setLayout(jPanel43Layout);
    jPanel43Layout.setHorizontalGroup(
        jPanel43Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel43Layout.createSequentialGroup()
            .addComponent(jTabbedPane14, javax.swing.GroupLayout.PREFERRED_SIZE, 741, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel43Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jScrollPane21, javax.swing.GroupLayout.DEFAULT_SIZE, 441, Short.MAX_VALUE)
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel43Layout.createSequentialGroup()
                    .addGap(0, 0, Short.MAX_VALUE)
                    .addComponent(addEventButton)
                    .addGap(0, 0, Short.MAX_VALUE)))
            .addContainerGap())
    );
    jPanel43Layout.setVerticalGroup(
        jPanel43Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel43Layout.createSequentialGroup()
            .addContainerGap()
            .addComponent(addEventButton)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(jScrollPane21))
        .addComponent(jTabbedPane14)
    );

    javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
    jPanel5.setLayout(jPanel5Layout);
    jPanel5Layout.setHorizontalGroup(
        jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGap(0, 1219, Short.MAX_VALUE)
        .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel43, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    jPanel5Layout.setVerticalGroup(
        jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGap(0, 757, Short.MAX_VALUE)
        .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addComponent(jPanel43, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap()))
    );

    jTabbedPane1.addTab("Eventos", jPanel5);

    jTabbedPane16.setTabPlacement(javax.swing.JTabbedPane.LEFT);
    jTabbedPane16.setFont(new java.awt.Font("Calibri Light", 0, 14)); // NOI18N

    jPanel48.setBackground(new java.awt.Color(255, 255, 255));

    jLabel125.setText("Nome:");

    jLabel126.setText("Morada:");

    jLabel127.setText("NIF:");

    jLabel128.setText("Observações:");

    jLabel18.setText("Atividade:");

    jLabel42.setText("Tipo:");

    editDonorButton.setBackground(new java.awt.Color(22, 113, 204));
    editDonorButton.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    editDonorButton.setForeground(new java.awt.Color(255, 255, 255));
    editDonorButton.setText("Editar");
    editDonorButton.setBorderPainted(false);
    editDonorButton.setContentAreaFilled(false);
    editDonorButton.setOpaque(true);
    editDonorButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            editDonorButtonActionPerformed(evt);
        }
    });

    removeDonorButton.setBackground(new java.awt.Color(22, 113, 204));
    removeDonorButton.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    removeDonorButton.setForeground(new java.awt.Color(255, 255, 255));
    removeDonorButton.setText("Remover");
    removeDonorButton.setBorderPainted(false);
    removeDonorButton.setContentAreaFilled(false);
    removeDonorButton.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR));
    removeDonorButton.setOpaque(true);

    jLabel2.setText("Contactos:");

    donorObservation.setEditable(false);

    donorObservation.setLineWrap(true);

    donorObservation.setToolTipText("");

    donorObservation.setWrapStyleWord(true);

    donorObservation.setDocument(new JTextAreaLimit(500));
    donorObservation.setColumns(20);
    donorObservation.setRows(5);
    jScrollPane3.setViewportView(donorObservation);

    donorName.setEditable(false);

    donorAddress.setEditable(false);

    donorNIF.setEditable(false);

    donorActivity.setEditable(false);

    donorType.setEditable(false);

    donorContacts.setAutoCreateRowSorter(true);
    donorContacts.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Tipo", "Contacto"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.String.class, java.lang.String.class
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }
    });
    donorContacts.setEnabled(false);
    donorContacts.getTableHeader().setReorderingAllowed(false);
    jScrollPane23.setViewportView(donorContacts);
    if (donorContacts.getColumnModel().getColumnCount() > 0) {
        donorContacts.getColumnModel().getColumn(0).setCellEditor(new DefaultCellEditor(
            new javax.swing.JComboBox(
                new javax.swing.DefaultComboBoxModel(
                    new String[] { "Telefone", "Telemóvel", "Email", "Fax" }))) );
    }

    addDonorContact.setBackground(new java.awt.Color(22, 113, 204));
    addDonorContact.setFont(new java.awt.Font("Noto Sans", 0, 14)); // NOI18N
    addDonorContact.setForeground(new java.awt.Color(254, 254, 254));
    addDonorContact.setText("+");
    addDonorContact.setBorderPainted(false);
    addDonorContact.setContentAreaFilled(false);
    addDonorContact.setOpaque(true);
    addDonorContact.setVisible(false);
    addDonorContact.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addDonorContactActionPerformed(evt);
        }
    });

    deleteDonorContact.setBackground(new java.awt.Color(22, 113, 204));
    deleteDonorContact.setFont(new java.awt.Font("Noto Sans", 0, 14)); // NOI18N
    deleteDonorContact.setForeground(new java.awt.Color(254, 254, 254));
    deleteDonorContact.setText("-");
    deleteDonorContact.setBorderPainted(false);
    deleteDonorContact.setContentAreaFilled(false);
    deleteDonorContact.setMaximumSize(new java.awt.Dimension(41, 23));
    deleteDonorContact.setMinimumSize(new java.awt.Dimension(41, 23));
    deleteDonorContact.setOpaque(true);
    deleteDonorContact.setVisible(false);
    deleteDonorContact.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            deleteDonorContactActionPerformed(evt);
        }
    });

    cancelDonorEdit.setVisible(false);
    cancelDonorEdit.setBackground(new java.awt.Color(22, 113, 204));
    cancelDonorEdit.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    cancelDonorEdit.setForeground(new java.awt.Color(255, 255, 255));
    cancelDonorEdit.setText("Cancelar");
    cancelDonorEdit.setBorderPainted(false);
    cancelDonorEdit.setContentAreaFilled(false);
    cancelDonorEdit.setOpaque(true);
    cancelDonorEdit.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            cancelDonorEditActionPerformed(evt);
        }
    });

    submitDonorEdit.setVisible(false);
    submitDonorEdit.setBackground(new java.awt.Color(22, 113, 204));
    submitDonorEdit.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    submitDonorEdit.setForeground(new java.awt.Color(255, 255, 255));
    submitDonorEdit.setText("Submeter");
    submitDonorEdit.setBorderPainted(false);
    submitDonorEdit.setContentAreaFilled(false);
    submitDonorEdit.setOpaque(true);

    javax.swing.GroupLayout jPanel48Layout = new javax.swing.GroupLayout(jPanel48);
    jPanel48.setLayout(jPanel48Layout);
    jPanel48Layout.setHorizontalGroup(
        jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel48Layout.createSequentialGroup()
            .addGap(15, 15, 15)
            .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                .addComponent(jLabel127)
                .addComponent(jLabel126)
                .addComponent(jLabel125)
                .addComponent(jLabel18)
                .addComponent(jLabel42)
                .addComponent(jLabel2)
                .addComponent(jLabel128))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                .addGroup(jPanel48Layout.createSequentialGroup()
                    .addComponent(cancelDonorEdit)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(submitDonorEdit)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(editDonorButton)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(removeDonorButton))
                .addComponent(donorAddress)
                .addComponent(donorName, javax.swing.GroupLayout.DEFAULT_SIZE, 484, Short.MAX_VALUE)
                .addComponent(donorNIF, javax.swing.GroupLayout.PREFERRED_SIZE, 199, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(donorActivity, javax.swing.GroupLayout.PREFERRED_SIZE, 199, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(donorType, javax.swing.GroupLayout.PREFERRED_SIZE, 199, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel48Layout.createSequentialGroup()
                    .addComponent(addDonorContact, javax.swing.GroupLayout.PREFERRED_SIZE, 31, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(deleteDonorContact, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGap(9, 9, 9))
                .addComponent(jScrollPane3)
                .addComponent(jScrollPane23))
            .addContainerGap(44, Short.MAX_VALUE))
    );
    jPanel48Layout.setVerticalGroup(
        jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel48Layout.createSequentialGroup()
            .addGap(20, 20, 20)
            .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(donorName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel125))
            .addGap(5, 5, 5)
            .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel126)
                .addComponent(donorAddress, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel127)
                .addComponent(donorNIF, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
            .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel18)
                .addComponent(donorActivity, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
            .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(jLabel42, javax.swing.GroupLayout.PREFERRED_SIZE, 17, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(donorType, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGap(18, 18, 18)
            .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(deleteDonorContact, javax.swing.GroupLayout.PREFERRED_SIZE, 26, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(addDonorContact, javax.swing.GroupLayout.PREFERRED_SIZE, 26, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGap(2, 2, 2)
            .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jScrollPane23, javax.swing.GroupLayout.PREFERRED_SIZE, 76, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel2))
            .addGap(18, 18, 18)
            .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, 101, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel128))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(removeDonorButton)
                .addComponent(cancelDonorEdit)
                .addGroup(jPanel48Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(editDonorButton)
                    .addComponent(submitDonorEdit)))
            .addGap(27, 27, 27))
    );

    jTabbedPane16.addTab("Informações", jPanel48);

    jPanel9.setBackground(new java.awt.Color(255, 255, 255));

    donorContacts.setEnabled(false);
    addDonationTable.setAutoCreateRowSorter(true);
    addDonationTable.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {

        },
        new String [] {
            "Data", "Tipo", "Valor"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.String.class, java.lang.String.class, java.lang.String.class
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }
    });
    jScrollPane24.setViewportView(addDonationTable);

    removeDonationButton.setBackground(new java.awt.Color(22, 113, 204));
    removeDonationButton.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    removeDonationButton.setForeground(new java.awt.Color(255, 255, 255));
    removeDonationButton.setText("Remover");
    removeDonationButton.setBorderPainted(false);
    removeDonationButton.setContentAreaFilled(false);
    removeDonationButton.setOpaque(true);
    removeDonationButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            removeDonationButtonActionPerformed(evt);
        }
    });

    addDonationButton.setBackground(new java.awt.Color(22, 113, 204));
    addDonationButton.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    addDonationButton.setForeground(new java.awt.Color(255, 255, 255));
    addDonationButton.setText("Adicionar");
    addDonationButton.setBorderPainted(false);
    addDonationButton.setContentAreaFilled(false);
    addDonationButton.setOpaque(true);
    addDonationButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addDonationButtonActionPerformed(evt);
        }
    });

    confirmRemoveDonationButton.setVisible(false);
    confirmRemoveDonationButton.setBackground(new java.awt.Color(22, 113, 204));
    confirmRemoveDonationButton.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    confirmRemoveDonationButton.setForeground(new java.awt.Color(255, 255, 255));
    confirmRemoveDonationButton.setText("Confirmar");
    confirmRemoveDonationButton.setBorderPainted(false);
    confirmRemoveDonationButton.setContentAreaFilled(false);
    confirmRemoveDonationButton.setOpaque(true);
    confirmRemoveDonationButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            confirmRemoveDonationButtonActionPerformed(evt);
        }
    });

    cancelRemoveDonationButton.setVisible(false);
    cancelRemoveDonationButton.setBackground(new java.awt.Color(22, 113, 204));
    cancelRemoveDonationButton.setFont(new java.awt.Font("Lucida Grande", 0, 14)); // NOI18N
    cancelRemoveDonationButton.setForeground(new java.awt.Color(255, 255, 255));
    cancelRemoveDonationButton.setText("Cancelar");
    cancelRemoveDonationButton.setBorderPainted(false);
    cancelRemoveDonationButton.setContentAreaFilled(false);
    cancelRemoveDonationButton.setOpaque(true);
    cancelRemoveDonationButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            cancelRemoveDonationButtonActionPerformed(evt);
        }
    });

    javax.swing.GroupLayout jPanel9Layout = new javax.swing.GroupLayout(jPanel9);
    jPanel9.setLayout(jPanel9Layout);
    jPanel9Layout.setHorizontalGroup(
        jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel9Layout.createSequentialGroup()
            .addGap(54, 54, 54)
            .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                .addComponent(jScrollPane24, javax.swing.GroupLayout.PREFERRED_SIZE, 515, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGroup(jPanel9Layout.createSequentialGroup()
                    .addComponent(cancelRemoveDonationButton)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(confirmRemoveDonationButton)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(removeDonationButton)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(addDonationButton)))
            .addGap(78, 78, 78))
    );
    jPanel9Layout.setVerticalGroup(
        jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel9Layout.createSequentialGroup()
            .addGap(40, 40, 40)
            .addComponent(jScrollPane24, javax.swing.GroupLayout.DEFAULT_SIZE, 620, Short.MAX_VALUE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(jPanel9Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(addDonationButton)
                .addComponent(confirmRemoveDonationButton)
                .addComponent(removeDonationButton)
                .addComponent(cancelRemoveDonationButton))
            .addGap(35, 35, 35))
    );

    jTabbedPane16.addTab("Donativos", jPanel9);

    addDonorButton.setBackground(new java.awt.Color(22, 113, 204));
    addDonorButton.setFont(new java.awt.Font("Lucida Grande", 0, 18)); // NOI18N
    addDonorButton.setForeground(new java.awt.Color(255, 255, 255));
    addDonorButton.setText("Adicionar doador");
    addDonorButton.setBorderPainted(false);
    addDonorButton.setContentAreaFilled(false);
    addDonorButton.setOpaque(true);
    addDonorButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            addDonorButtonActionPerformed(evt);
        }
    });

    jButton40.setBackground(new java.awt.Color(22, 113, 204));
    jButton40.setFont(new java.awt.Font("Lucida Grande", 0, 18)); // NOI18N
    jButton40.setForeground(new java.awt.Color(255, 255, 255));
    jButton40.setText("Anónimos");
    jButton40.setBorderPainted(false);
    jButton40.setContentAreaFilled(false);
    jButton40.setOpaque(true);

    jTable2.setAutoCreateRowSorter(true);
    jTable2.setModel(new javax.swing.table.DefaultTableModel(
        new Object [][] {
            {null, null, null},
            {null, null, null},
            {null, null, null}
        },
        new String [] {
            "Nome", "Tipo", "Atividade"
        }
    ) {
        Class[] types = new Class [] {
            java.lang.String.class, java.lang.String.class, java.lang.String.class
        };
        boolean[] canEdit = new boolean [] {
            false, false, false
        };

        public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
        }

        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
        }
    });
    jTable2.getTableHeader().setReorderingAllowed(false);
    jScrollPane25.setViewportView(jTable2);

    javax.swing.GroupLayout jPanel47Layout = new javax.swing.GroupLayout(jPanel47);
    jPanel47.setLayout(jPanel47Layout);
    jPanel47Layout.setHorizontalGroup(
        jPanel47Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel47Layout.createSequentialGroup()
            .addComponent(jTabbedPane16, javax.swing.GroupLayout.PREFERRED_SIZE, 740, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addGroup(jPanel47Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addGroup(jPanel47Layout.createSequentialGroup()
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(jPanel47Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel47Layout.createSequentialGroup()
                            .addGap(0, 0, Short.MAX_VALUE)
                            .addComponent(jButton40)
                            .addGap(0, 0, Short.MAX_VALUE))
                        .addComponent(jScrollPane25, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE))
                    .addContainerGap())
                .addGroup(jPanel47Layout.createSequentialGroup()
                    .addGap(155, 155, 155)
                    .addComponent(addDonorButton)
                    .addContainerGap(162, Short.MAX_VALUE))))
    );
    jPanel47Layout.setVerticalGroup(
        jPanel47Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel47Layout.createSequentialGroup()
            .addContainerGap()
            .addComponent(addDonorButton)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(jButton40)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(jScrollPane25))
        .addComponent(jTabbedPane16)
    );

    javax.swing.GroupLayout jPanel8Layout = new javax.swing.GroupLayout(jPanel8);
    jPanel8.setLayout(jPanel8Layout);
    jPanel8Layout.setHorizontalGroup(
        jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGap(0, 1219, Short.MAX_VALUE)
        .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel47, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );
    jPanel8Layout.setVerticalGroup(
        jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGap(0, 757, Short.MAX_VALUE)
        .addGroup(jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addComponent(jPanel47, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap()))
    );

    jTabbedPane1.addTab("Doadores", jPanel8);

    jPanel13.setBackground(new java.awt.Color(22, 113, 204));

    jLabel7.setFont(new java.awt.Font("Calibri Light", 0, 24)); // NOI18N
    jLabel7.setForeground(new java.awt.Color(255, 255, 255));
    jLabel7.setText(" Habitat For Humanity");
    jLabel7.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(255, 255, 255), 2));
    jLabel7.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);

    search.setBackground(new java.awt.Color(254, 254, 254));
    search.setFont(new java.awt.Font("Lucida Grande", 0, 18)); // NOI18N
    search.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(255, 255, 255), 3));
    search.setPreferredSize(new java.awt.Dimension(6, 25));
    search.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            searchActionPerformed(evt);
        }
    });

    jLabel6.setFont(new java.awt.Font("Lucida Grande", 0, 20)); // NOI18N
    jLabel6.setForeground(new java.awt.Color(255, 255, 255));
    jLabel6.setText("Pesquisa");

    clearSearch.setForeground(new java.awt.Color(255, 255, 255));
    clearSearch.setText("Limpar");
    clearSearch.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(255, 255, 255), 2));
    clearSearch.setContentAreaFilled(false);
    clearSearch.setPreferredSize(new java.awt.Dimension(47, 25));
    clearSearch.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
            clearSearchActionPerformed(evt);
        }
    });

    javax.swing.GroupLayout jPanel13Layout = new javax.swing.GroupLayout(jPanel13);
    jPanel13.setLayout(jPanel13Layout);
    jPanel13Layout.setHorizontalGroup(
        jPanel13Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(jPanel13Layout.createSequentialGroup()
            .addGap(27, 27, 27)
            .addComponent(jLabel7, javax.swing.GroupLayout.PREFERRED_SIZE, 267, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addGap(304, 304, 304)
            .addComponent(jLabel6)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(search, javax.swing.GroupLayout.DEFAULT_SIZE, 445, Short.MAX_VALUE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(clearSearch, javax.swing.GroupLayout.PREFERRED_SIZE, 58, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addGap(18, 18, 18))
    );
    jPanel13Layout.setVerticalGroup(
        jPanel13Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel13Layout.createSequentialGroup()
            .addGap(0, 0, Short.MAX_VALUE)
            .addGroup(jPanel13Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                .addComponent(search, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addComponent(jLabel6))
            .addContainerGap())
        .addGroup(jPanel13Layout.createSequentialGroup()
            .addContainerGap()
            .addGroup(jPanel13Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jLabel7, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE)
                .addGroup(jPanel13Layout.createSequentialGroup()
                    .addComponent(clearSearch, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGap(7, 7, 7))))
    );

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addComponent(jTabbedPane1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        .addComponent(jPanel13, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
    );
    layout.setVerticalGroup(
        layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addGroup(layout.createSequentialGroup()
            .addComponent(jPanel13, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addGap(0, 0, 0)
            .addComponent(jTabbedPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 803, Short.MAX_VALUE))
    );

    pack();
    }// </editor-fold>//GEN-END:initComponents

    private void addDonorButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addDonorButtonActionPerformed
        (new AdicionarDoador(this, true)).setVisible(true);
    }//GEN-LAST:event_addDonorButtonActionPerformed

    private void cancelRemoveDonationButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelRemoveDonationButtonActionPerformed
        donorContacts.setEnabled(false);
        confirmRemoveDonationButton.setVisible(false);
        cancelRemoveDonationButton.setVisible(false);
        removeDonationButton.setVisible(true);
        addDonationButton.setVisible(true);
    }//GEN-LAST:event_cancelRemoveDonationButtonActionPerformed

    private void confirmRemoveDonationButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_confirmRemoveDonationButtonActionPerformed
        int rowID;
        if(( rowID = addDonationTable.getSelectedRow()) >= 0)
        ((DefaultTableModel)addDonationTable.getModel()).removeRow(rowID);

        confirmRemoveDonationButton.setVisible(false);
        cancelRemoveDonationButton.setVisible(false);
        removeDonationButton.setVisible(true);
        addDonationButton.setVisible(true);
    }//GEN-LAST:event_confirmRemoveDonationButtonActionPerformed

    private void addDonationButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addDonationButtonActionPerformed
        (new AdicionarDonativoaDoador(this, true)).setVisible(true);
    }//GEN-LAST:event_addDonationButtonActionPerformed

    private void removeDonationButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_removeDonationButtonActionPerformed
        donorContacts.setEnabled(true);
        removeDonationButton.setVisible(false);
        addDonationButton.setVisible(false);
        confirmRemoveDonationButton.setVisible(true);
        cancelRemoveDonationButton.setVisible(true);
    }//GEN-LAST:event_removeDonationButtonActionPerformed

    private void cancelDonorEditActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelDonorEditActionPerformed
        donorName.setEditable(false);
        donorAddress.setEditable(false);
        donorNIF.setEditable(false);
        donorType.setEditable(false);
        donorActivity.setEditable(false);
        donorObservation.setEditable(false);
        donorContacts.setEnabled(false);
        cancelDonorEdit.setVisible(false);
        submitDonorEdit.setVisible(false);
        addDonorContact.setVisible(false);
        deleteDonorContact.setVisible(false);
    }//GEN-LAST:event_cancelDonorEditActionPerformed

    private void deleteDonorContactActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteDonorContactActionPerformed
        int rowID;
        if(( rowID = donorContacts.getSelectedRow()) >= 0)
        ((DefaultTableModel)donorContacts.getModel()).removeRow(rowID);
    }//GEN-LAST:event_deleteDonorContactActionPerformed

    private void addDonorContactActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addDonorContactActionPerformed
        ((DefaultTableModel)donorContacts.getModel()).addRow(new Object[]{"", "", ""});
    }//GEN-LAST:event_addDonorContactActionPerformed

    private void editDonorButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editDonorButtonActionPerformed
        donorName.setEditable(true);
        donorAddress.setEditable(true);
        donorNIF.setEditable(true);
        donorType.setEditable(true);
        donorActivity.setEditable(true);
        donorObservation.setEditable(true);
        donorContacts.setEnabled(true);
        cancelDonorEdit.setVisible(true);
        submitDonorEdit.setVisible(true);
        addDonorContact.setVisible(true);
        deleteDonorContact.setVisible(true);
    }//GEN-LAST:event_editDonorButtonActionPerformed

    private void addEventButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addEventButtonActionPerformed
        (new AdicionarEvento(this, true)).setVisible(true);
    }//GEN-LAST:event_addEventButtonActionPerformed

    private void addEventButtonMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_addEventButtonMouseClicked
        (new AdicionarEvento(this, true)).setVisible(true);
    }//GEN-LAST:event_addEventButtonMouseClicked

    private void editParticipantsButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editParticipantsButtonActionPerformed
        (new EditarParticipantes(this, true)).setVisible(true);
    }//GEN-LAST:event_editParticipantsButtonActionPerformed

    private void cancelEditEventButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelEditEventButtonActionPerformed
        eventAddress.setEditable(false);
        eventDate.setEditable(false);
        eventRaisedValue.setEditable(false);
        eventParticipantNmb.setEnabled(false);
        eventObservation.setEditable(false);
        cancelEditEventButton.setVisible(false);
        submitEditEventButton.setVisible(false);
    }//GEN-LAST:event_cancelEditEventButtonActionPerformed

    private void eventRaisedValueActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_eventRaisedValueActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_eventRaisedValueActionPerformed

    private void eventAddressActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_eventAddressActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_eventAddressActionPerformed

    private void eventDateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_eventDateActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_eventDateActionPerformed

    private void removeEventButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_removeEventButtonActionPerformed

    }//GEN-LAST:event_removeEventButtonActionPerformed

    private void editEventButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editEventButtonActionPerformed
        eventAddress.setEditable(true);
        eventDate.setEditable(true);
        eventRaisedValue.setEditable(true);
        eventParticipantNmb.setEnabled(true);
        eventObservation.setEditable(true);
        submitEditEventButton.setVisible(true);
        cancelEditEventButton.setVisible(true);
    }//GEN-LAST:event_editEventButtonActionPerformed

    private void addVolunteerActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addVolunteerActionPerformed
        AdicionarVoluntario window = new AdicionarVoluntario(this, true);
        try {
            window.setActivities();
            window.setVisible(true);
        } catch (DataException ex) {
            JOptionPane.showMessageDialog(this, "Ocorreu um erro ao obter os dados");
        }

    }//GEN-LAST:event_addVolunteerActionPerformed

    private void addVolunteerMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_addVolunteerMouseClicked
        (new AdicionarVoluntario(this, true)).setVisible(true);
    }//GEN-LAST:event_addVolunteerMouseClicked

    private void submitEditVolunteerActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_submitEditVolunteerActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_submitEditVolunteerActionPerformed

    private void cancelEditVolunteerActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelEditVolunteerActionPerformed
        volunteerName.setEditable(false);
        volunteerAddress.setEditable(false);
        volunteerNotes.setEditable(false);
        volunteerBirthDate.setEditable(false);
        volunteerEducation.setEditable(false);
        volunteerMaritalStatus.setEnabled(false);
        volunteerNif.setEditable(false);
        volunteerNib.setEditable(false);
        cancelEditVolunteer.setVisible(false);
        submitEditVolunteer.setVisible(false);
        addVolunteerTeam.setVisible(false);
        volunteerNationality.setEnabled(false);
        volunteerBirthPlace.setEditable(false);
        volunteerTeam.setEnabled(false);
    }//GEN-LAST:event_cancelEditVolunteerActionPerformed

    private void volunteerNationalityActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_volunteerNationalityActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_volunteerNationalityActionPerformed

    private void volunteerBirthDateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_volunteerBirthDateActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_volunteerBirthDateActionPerformed

    private void editVolunteerActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editVolunteerActionPerformed
        volunteerName.setEditable(true);
        volunteerAddress.setEditable(true);
        volunteerNotes.setEditable(true);
        volunteerBirthDate.setEditable(true);
        volunteerEducation.setEditable(true);
        volunteerMaritalStatus.setEnabled(true);
        volunteerNationality.setEnabled(true);
        volunteerBirthPlace.setEditable(true);
        volunteerTeam.setEnabled(true);
        volunteerNif.setEditable(true);
        volunteerNib.setEditable(true);
        cancelEditVolunteer.setVisible(true);
        submitEditVolunteer.setVisible(true);
        addVolunteerTeam.setVisible(true);
    }//GEN-LAST:event_editVolunteerActionPerformed

    private void addProjectActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addProjectActionPerformed
        (new AdicionarProjeto(this, true)).setVisible(true);
    }//GEN-LAST:event_addProjectActionPerformed

    private void removeTaskActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_removeTaskActionPerformed
        Integer rowID;
        if(( rowID = taskList.getSelectedRow()) >= 0)
        ((DefaultTableModel)taskList.getModel()).removeRow(rowID);
    }//GEN-LAST:event_removeTaskActionPerformed

    private void addTaskActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addTaskActionPerformed
        ((DefaultTableModel)taskList.getModel()).addRow(new Object[]{"", "", ""});
    }//GEN-LAST:event_addTaskActionPerformed

    private void deletePaymentActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deletePaymentActionPerformed
        Integer rowID;
        if(( rowID = paymentPlan.getSelectedRow()) >= 0)
        ((DefaultTableModel)paymentPlan.getModel()).removeRow(rowID);
    }//GEN-LAST:event_deletePaymentActionPerformed

    private void addPaymentActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addPaymentActionPerformed
        ((DefaultTableModel)paymentPlan.getModel()).addRow(new Object[]{"", "", ""});
    }//GEN-LAST:event_addPaymentActionPerformed

    private void projectNameActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_projectNameActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_projectNameActionPerformed

    private void cancelEditProjectActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelEditProjectActionPerformed
        projectName.setEditable(false);
        projectStartDate.setEditable(false);
        projectEta.setEditable(false);
        jtfPDF.setEditable(false);
        jtfPDEC.setEditable(false);
        projectSignDate.setEditable(false);
        projectBudget.setEditable(false);
        projectFinalCost.setEditable(false);
        jtfPVP.setEditable(false);
        projectNotes.setEditable(false);
        cancelEditProject.setVisible(false);
        submitEditProject.setVisible(false);
        //TODO - Refresh from DB
    }//GEN-LAST:event_cancelEditProjectActionPerformed

    private void editProjectActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editProjectActionPerformed
        projectName.setEditable(true);
        projectStartDate.setEditable(true);
        projectEta.setEditable(true);
        jtfPDF.setEditable(true);
        jtfPDEC.setEditable(true);
        projectSignDate.setEditable(true);
        projectBudget.setEditable(true);
        projectFinalCost.setEditable(true);
        jtfPVP.setEditable(true);
        projectNotes.setEditable(true);
        cancelEditProject.setVisible(true);
        submitEditProject.setVisible(true);
    }//GEN-LAST:event_editProjectActionPerformed

    private void addFamilyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addFamilyActionPerformed
        AdicionarFamilia af = new AdicionarFamilia(this, true);
        try {
            af.setActivities();
            af.setVisible(true);
        } catch (DataException ex) {
            JOptionPane.showMessageDialog(this, "Ocorreu um erro ao obter os dados");
        }
                
    }//GEN-LAST:event_addFamilyActionPerformed

    private void addMemberActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addMemberActionPerformed
        ((DefaultTableModel)memberList.getModel()).addRow(new Object[]{"", "", ""});
    }//GEN-LAST:event_addMemberActionPerformed

    private void addApplicationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addApplicationActionPerformed
        if (currentFamily == null) {           
                JOptionPane.showMessageDialog(this, "Por favor seleccione uma família.");
                return;
        }
            
        (new AdicionarCandidatura(this, true, currentFamily.getId())).setVisible(true);
    }//GEN-LAST:event_addApplicationActionPerformed

    private void repNationalityActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_repNationalityActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_repNationalityActionPerformed

    private void disableFamilyEdit() {
        familyName.setEditable(false);
        familyAddress.setEditable(false);
        familyIncome.setEditable(false);
        familyNotes.setEditable(false);
        familyRep.setEditable(false);
        repBirthDate.setEditable(false);
        repEducation.setEditable(false);
        repMaritalStatus.setEnabled(false);
        repNif.setEditable(false);
        repNib.setEditable(false);
        repContacts.setEnabled(false);
        cancelEditFamily.setVisible(false);
        submitEditFamily.setVisible(false);
        addRepContact.setVisible(false);
        deleteRepContact.setVisible(false);
        repNationality.setEnabled(false);
        repBirthPlace.setEditable(false);
        mainWindowRepProf.setEnabled(false);
    }
    private void cancelEditFamilyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelEditFamilyActionPerformed
        disableFamilyEdit();
    }//GEN-LAST:event_cancelEditFamilyActionPerformed

    private void addRepContactActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addRepContactActionPerformed
        ((DefaultTableModel)repContacts.getModel()).addRow(new Object[]{"", "", ""});
    }//GEN-LAST:event_addRepContactActionPerformed

    private void deleteRepContactActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteRepContactActionPerformed
        int row = repContacts.getSelectedRow();
        if(row == -1)
            return;
        Contact c = representativeContacts.get(row);
        representativeContacts.remove(row);
        ((DefaultTableModel)repContacts.getModel()).removeRow(row);
        try {
            ControllerFactory.getContactsController().delete(c);
        } catch(DataException e) {
            JOptionPane.showMessageDialog(this, "Ocorreu um erro ao remover");
        }
    }//GEN-LAST:event_deleteRepContactActionPerformed

    private void repBirthDateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_repBirthDateActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_repBirthDateActionPerformed

    private void editFamilyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editFamilyActionPerformed
        if(currentFamily == null) {
                JOptionPane.showMessageDialog(this, "Por favor seleccione uma família.");
                return;
        }
                    
        familyName.setEditable(true);
        familyAddress.setEditable(true);
        familyIncome.setEditable(true);
        familyNotes.setEditable(true);
        familyRep.setEditable(true);
        repBirthDate.setEditable(true);
        repEducation.setEditable(true);
        repMaritalStatus.setEnabled(true);
        repNif.setEditable(true);
        repNib.setEditable(true);
        repNationality.setEnabled(true);
        repBirthPlace.setEditable(true);
        repContacts.setEnabled(true);
        cancelEditFamily.setVisible(true);
        submitEditFamily.setVisible(true);
        addRepContact.setVisible(true);
        deleteRepContact.setVisible(true);
        familyVolHours.setVisible(true);
        mainWindowRepProf.setEnabled(true);
    }//GEN-LAST:event_editFamilyActionPerformed

    private void deleteFamilyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteFamilyActionPerformed
        try {
            if(currentFamily == null) {
                JOptionPane.showMessageDialog(this, "Por favor seleccione uma família.");
                return;
            }
            
            if(!confirmPassword()) {
                JOptionPane.showMessageDialog(this, "Password errada");
                return;
            }

            ControllerFactory.getFamiliesController().delete(currentFamily);
            
            familyList.remove(familyList.getSelectedRow());
            
            if(familyList.getRowCount() > 0)
                familyList.getSelectionModel().setSelectionInterval(0, 0);
        } catch (DataException ex) {
            JOptionPane.showMessageDialog(this, "Erro a ler dados");
        }
    }//GEN-LAST:event_deleteFamilyActionPerformed

    private void editRepresentativeContacts() {
        int rowCount = repContacts.getRowCount();
        
        if(rowCount <= 0)
            return;
        
        try {
            Controller<Contact> cc = ControllerFactory.getContactsController();
        
            int nrContacts = representativeContacts.size();
            int i = 0;
        
            final TableModel t = repContacts.getModel();
            while (i < nrContacts) {
                Contact c = representativeContacts.get(i);
                c.setType(t.getValueAt(i, 0).toString());
                c.setValue(t.getValueAt(i, 1).toString());
                cc.save(c);
                i++;
            }
        
            while(i < rowCount) {
                final int i2 = i;
                Contact newContact = cc.save( new HashMap<String, Object>() {{
                    put("type", t.getValueAt(i2, 0).toString());
                    put("value", t.getValueAt(i2, 1).toString());
                    put("ownerType", "Representante");
                    put("owner", currentRepresentative.getId());
                }});
                i++;
            }    
        } catch (DataException e) {
            JOptionPane.showMessageDialog(this, "Erro a gravar dados");
        }
    }
    
    
    private void submitEditFamilyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_submitEditFamilyActionPerformed
        if(currentFamily == null) {
            JOptionPane.showMessageDialog(this, "Por favor seleccione uma família.");
            return;
        }
        
        Controller rc = ControllerFactory.getRepresentativesController();
        Controller mc = ControllerFactory.getMembersController();
        Controller fc = ControllerFactory.getFamiliesController();
        Controller cc = ControllerFactory.getContactsController();
        
        try {
            currentFamily.setName(familyName.getText());
            currentFamily.setAddress(familyAddress.getText());
            currentFamily.setObservations(familyNotes.getText());
            currentFamily.setIncome(Float.parseFloat(familyIncome.getText()));
            
            fc.save(currentFamily);
            
            currentRepresentative.setName(familyRep.getText());
            currentRepresentative.setBirthDate(Util.strToDate( repBirthDate.getText() ));
            currentRepresentative.setMaritalStatus(repMaritalStatus.getSelectedItem().toString());
            currentRepresentative.setEducation(repEducation.getText());
            currentRepresentative.setActivity((Activity)mainWindowRepProf.getSelectedItem());
            currentRepresentative.setNif(repNif.getText());
            currentRepresentative.setNib(repNib.getText());
            
            editRepresentativeContacts();
            rc.save(currentRepresentative);
        } catch (DataException e) {
            JOptionPane.showMessageDialog(this, "Erro a guardar dados");
        }finally{
        disableFamilyEdit();
        }
    }//GEN-LAST:event_submitEditFamilyActionPerformed

    private void editQuestionnaireSubmitActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editQuestionnaireSubmitActionPerformed
        int rowCount = applicationQuestionnaire.getRowCount();
        if (rowCount <= 0){
            JOptionPane.showMessageDialog(this, "Por favor responda a todas as perguntas");
            return;
        }
        
        try {
            ApplicationsController ac = (ApplicationsController) ControllerFactory.getApplicationsController();        
            TableModel t = applicationQuestionnaire.getModel();
            
            for(int i = 0; i < rowCount; i++) {
                if(t.getValueAt(i, 1).toString().length() == 0) {
                    JOptionPane.showMessageDialog(this, "Por favor responda a todas as perguntas");
                    return;
                }
            }
            
            for(int i = 0; i < rowCount; i++) {
                ac.addAnswerTo((Question)t.getValueAt(i, 0), currentApplication, t.getValueAt(i, 1).toString());
            }    
        } catch (DataException e) {
            JOptionPane.showMessageDialog(this, "Erro a gravar dados");
        }finally{
            editQuestionnaireCancel.setVisible(false);
            editQuestionnaireSubmit.setVisible(false);
            applicationDate.setEditable(false);
            applicationPriority.setEditable(false);
            applicationLocation.setEditable(false);
            applicationApproved.setEnabled(false);
            applicationApprovalDate.setEditable(false);
            applicationNotes.setEditable(false);
            applicationQuestionnaire.setEnabled(false);
            applicationPriority.setEnabled(false);
            addApplication.setVisible(false);
            deleteApplication.setVisible(false);
            editApplication.setVisible(false);
        }
        
        
        currentApplication.setApplicationDate(Util.strToDate(applicationDate.getText()));
        currentApplication.setPriority(applicationPriority.getSelectedIndex());
        currentApplication.setNotes(applicationNotes.getText());
        currentApplication.setLocation(applicationLocation.getText());
        currentApplication.setStatus(applicationApproved.isSelected());
        currentApplication.setApprovalDate(Util.strToDate(applicationApprovalDate.getText()));
        
        try {
            ControllerFactory.getApplicationsController().save(currentApplication);
        } catch (DataException e) {
            JOptionPane.showMessageDialog(this, "Erro a gravar dados");
        }
    }//GEN-LAST:event_editQuestionnaireSubmitActionPerformed

    private void editApplicationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editApplicationActionPerformed
        if(currentFamily == null) {
                JOptionPane.showMessageDialog(this, "Por favor seleccione uma família.");
                return;
        }
        editQuestionnaireCancel.setVisible(true);
        editQuestionnaireSubmit.setVisible(true);
        applicationDate.setEditable(true);
        applicationPriority.setEditable(true);
        applicationLocation.setEditable(true);
        applicationApproved.setEnabled(true);
        applicationApprovalDate.setEditable(true);
        applicationNotes.setEditable(true);
        applicationQuestionnaire.setEnabled(true);
        applicationPriority.setEnabled(true);
        addApplication.setVisible(false);
        deleteApplication.setVisible(false);
        editApplication.setVisible(false);
    }//GEN-LAST:event_editApplicationActionPerformed

    private void submitMembersActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_submitMembersActionPerformed
        int rowCount = memberList.getRowCount();
        
        if(rowCount <= 0)
            return;
        
        try {
            Controller<SimpleMember> mc = ControllerFactory.getMembersController();
        
            int nrMembers = currentMembers.size();
            int i = 0;
        
            final TableModel t = memberList.getModel();
            while (i < nrMembers) {
                SimpleMember m = currentMembers.get(i);
                m.setName(t.getValueAt(i, 1).toString());
                m.setBirthDate(Util.strToDate(t.getValueAt(i, 2).toString()));
                m.setKinship(t.getValueAt(i, 3).toString());
                mc.save(m);
                i++;
            }
        
            while(i < rowCount) {
                final int i2 = i;
                SimpleMember newMember = mc.save( new HashMap<String, Object>() {{
                    put("name", t.getValueAt(i2, 1).toString());
                    put("birthDate", Util.strToDate(t.getValueAt(i2, 2).toString()));
                    put("kinship", t.getValueAt(i2, 3).toString());
                    put("familyID", currentFamily.getId());
                }});
                t.setValueAt(newMember.getId(), i2, 0);
                i++;
            }    
        } catch (DataException e) {
            JOptionPane.showMessageDialog(this, "Erro a gravar dados");
        }
    }//GEN-LAST:event_submitMembersActionPerformed

    private void removeMemberActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_removeMemberActionPerformed
        int row = memberList.getSelectedRow();
        if(row == -1)
            return;
        
        if(!confirmPassword()) {
            JOptionPane.showMessageDialog(this, "Password errada");
            return;
        }
        
        SimpleMember m = currentMembers.get(row);
        currentMembers.remove(row);
        ((DefaultTableModel)memberList.getModel()).removeRow(row);
        try {
            ControllerFactory.getMembersController().delete(m);
        } catch(DataException e) {
            JOptionPane.showMessageDialog(this, "Ocorreu um erro ao remover");
        }
    }//GEN-LAST:event_removeMemberActionPerformed

    private void editQuestionnaireCancelActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editQuestionnaireCancelActionPerformed
        editQuestionnaireCancel.setVisible(false);
        editQuestionnaireSubmit.setVisible(false);
        applicationDate.setEditable(false);
        applicationPriority.setEditable(false);
        applicationLocation.setEditable(false);
        applicationApproved.setEnabled(false);
        applicationApprovalDate.setEditable(false);
        applicationNotes.setEditable(false);
        applicationQuestionnaire.setEnabled(false);
        applicationPriority.setEnabled(false);
        addApplication.setVisible(true);
        deleteApplication.setVisible(true);
        editApplication.setVisible(true);
    }//GEN-LAST:event_editQuestionnaireCancelActionPerformed

    private void nextApplicationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_nextApplicationActionPerformed
        applicationIndex--;
        setCurrentApplication();
        if(applicationIndex == 0)
            nextApplication.setEnabled(false);
        
        previousApplication.setEnabled(true);
        
    }//GEN-LAST:event_nextApplicationActionPerformed

    private void previousApplicationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_previousApplicationActionPerformed
        applicationIndex++;
        setCurrentApplication();
        if(applicationIndex == familyApplications.size() - 1)
            previousApplication.setEnabled(false);
        
        nextApplication.setEnabled(true);
    }//GEN-LAST:event_previousApplicationActionPerformed

    private void searchActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_searchActionPerformed
        switch(jTabbedPane1.getSelectedIndex()){
            case 0: filterFamilies(search.getText());
                    break;
        }
    }//GEN-LAST:event_searchActionPerformed

    private void clearSearchActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_clearSearchActionPerformed
        search.setText("");
        listFamilies();
    }//GEN-LAST:event_clearSearchActionPerformed

    private void filterFamilies( String text){
        DefaultTableModel familyListModel = ((DefaultTableModel)familyList.getModel());
        boolean isNumber = true;
        int id = -1;
        try{
            id = Integer.parseInt(text);
        }catch(NumberFormatException e){
            isNumber = false;
        }
        
        if(text.length() == 0)
            listFamilies();
        else{
            tableloop:
            for(int i = 0; i < familyList.getRowCount(); i++){
                
                if(  isNumber && (id == ((int) familyList.getValueAt(i, 0) ) ))
                    continue;
                for(int j = 1; j < familyList.getColumnCount(); j++){
                    if( ( (String) familyList.getValueAt(i,j) ).contains(text))
                        continue tableloop;
                }
                System.out.println("Remove");
                familyListModel.removeRow(i);
                i--;
                
            }
        }
    }
        
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton addApplication;
    private javax.swing.JButton addDonationButton;
    private javax.swing.JTable addDonationTable;
    private javax.swing.JButton addDonorButton;
    private javax.swing.JButton addDonorContact;
    private javax.swing.JButton addEventButton;
    private javax.swing.JButton addFamily;
    private javax.swing.JButton addMember;
    private javax.swing.JButton addPayment;
    private javax.swing.JButton addProject;
    private javax.swing.JButton addRepContact;
    private javax.swing.JButton addTask;
    private javax.swing.JButton addVolunteer;
    private javax.swing.JButton addVolunteerTeam;
    private javax.swing.JFormattedTextField applicationApprovalDate;
    private javax.swing.JCheckBox applicationApproved;
    private javax.swing.JFormattedTextField applicationDate;
    private javax.swing.JTextField applicationId;
    private javax.swing.JTextField applicationLocation;
    private javax.swing.JTextArea applicationNotes;
    private javax.swing.JLabel applicationPages;
    private javax.swing.JComboBox applicationPriority;
    private javax.swing.JTable applicationQuestionnaire;
    private javax.swing.JButton cancelDonorEdit;
    private javax.swing.JButton cancelEditEventButton;
    private javax.swing.JButton cancelEditFamily;
    private javax.swing.JButton cancelEditProject;
    private javax.swing.JButton cancelEditVolunteer;
    private javax.swing.JButton cancelRemoveDonationButton;
    private javax.swing.JButton clearSearch;
    private javax.swing.JButton confirmRemoveDonationButton;
    private javax.swing.JButton deleteApplication;
    private javax.swing.JButton deleteDonorContact;
    private javax.swing.JButton deleteFamily;
    private javax.swing.JButton deletePayment;
    private javax.swing.JButton deleteProject;
    private javax.swing.JButton deleteRepContact;
    private javax.swing.JButton deleteVolunteer;
    private javax.swing.JTextField donorActivity;
    private javax.swing.JTextField donorAddress;
    private javax.swing.JTable donorContacts;
    private javax.swing.JTextField donorNIF;
    private javax.swing.JTextField donorName;
    private javax.swing.JTextArea donorObservation;
    private javax.swing.JTextField donorType;
    private javax.swing.JButton editApplication;
    private javax.swing.JButton editDonorButton;
    private javax.swing.JButton editEventButton;
    private javax.swing.JButton editFamily;
    private javax.swing.JButton editParticipantsButton;
    private javax.swing.JButton editPaymentPlan;
    private javax.swing.JButton editProject;
    private javax.swing.JButton editQuestionnaireCancel;
    private javax.swing.JButton editQuestionnaireSubmit;
    private javax.swing.JButton editVolunteer;
    private javax.swing.JTextField eventAddress;
    private javax.swing.JFormattedTextField eventDate;
    private javax.swing.JTextArea eventObservation;
    private javax.swing.JSpinner eventParticipantNmb;
    private javax.swing.JFormattedTextField eventRaisedValue;
    private javax.swing.JTable eventTable;
    private javax.swing.JTextField familyAddress;
    private javax.swing.JCheckBox familyApproved;
    private javax.swing.JFormattedTextField familyIncome;
    private javax.swing.JTable familyList;
    private javax.swing.JTextField familyName;
    private javax.swing.JTextArea familyNotes;
    private javax.swing.JTextField familyRep;
    private javax.swing.JTabbedPane familySubTabbedPane;
    private javax.swing.JTextField familyVolHours;
    private javax.swing.JButton jButton40;
    private javax.swing.JDialog jDialog1;
    private javax.swing.JFormattedTextField jFormattedTextField1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel110;
    private javax.swing.JLabel jLabel111;
    private javax.swing.JLabel jLabel112;
    private javax.swing.JLabel jLabel114;
    private javax.swing.JLabel jLabel115;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel121;
    private javax.swing.JLabel jLabel122;
    private javax.swing.JLabel jLabel123;
    private javax.swing.JLabel jLabel124;
    private javax.swing.JLabel jLabel125;
    private javax.swing.JLabel jLabel126;
    private javax.swing.JLabel jLabel127;
    private javax.swing.JLabel jLabel128;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel130;
    private javax.swing.JLabel jLabel131;
    private javax.swing.JLabel jLabel133;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel15;
    private javax.swing.JLabel jLabel16;
    private javax.swing.JLabel jLabel17;
    private javax.swing.JLabel jLabel18;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel20;
    private javax.swing.JLabel jLabel21;
    private javax.swing.JLabel jLabel22;
    private javax.swing.JLabel jLabel23;
    private javax.swing.JLabel jLabel24;
    private javax.swing.JLabel jLabel25;
    private javax.swing.JLabel jLabel26;
    private javax.swing.JLabel jLabel27;
    private javax.swing.JLabel jLabel28;
    private javax.swing.JLabel jLabel29;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel31;
    private javax.swing.JLabel jLabel32;
    private javax.swing.JLabel jLabel33;
    private javax.swing.JLabel jLabel35;
    private javax.swing.JLabel jLabel36;
    private javax.swing.JLabel jLabel37;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel42;
    private javax.swing.JLabel jLabel44;
    private javax.swing.JLabel jLabel46;
    private javax.swing.JLabel jLabel47;
    private javax.swing.JLabel jLabel48;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel58;
    private javax.swing.JLabel jLabel59;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel60;
    private javax.swing.JLabel jLabel61;
    private javax.swing.JLabel jLabel62;
    private javax.swing.JLabel jLabel63;
    private javax.swing.JLabel jLabel64;
    private javax.swing.JLabel jLabel65;
    private javax.swing.JLabel jLabel66;
    private javax.swing.JLabel jLabel67;
    private javax.swing.JLabel jLabel68;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel88;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel12;
    private javax.swing.JPanel jPanel13;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel24;
    private javax.swing.JPanel jPanel25;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel33;
    private javax.swing.JPanel jPanel34;
    private javax.swing.JPanel jPanel35;
    private javax.swing.JPanel jPanel36;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel43;
    private javax.swing.JPanel jPanel44;
    private javax.swing.JPanel jPanel45;
    private javax.swing.JPanel jPanel46;
    private javax.swing.JPanel jPanel47;
    private javax.swing.JPanel jPanel48;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JScrollPane jScrollPane10;
    private javax.swing.JScrollPane jScrollPane11;
    private javax.swing.JScrollPane jScrollPane12;
    private javax.swing.JScrollPane jScrollPane13;
    private javax.swing.JScrollPane jScrollPane14;
    private javax.swing.JScrollPane jScrollPane15;
    private javax.swing.JScrollPane jScrollPane16;
    private javax.swing.JScrollPane jScrollPane17;
    private javax.swing.JScrollPane jScrollPane18;
    private javax.swing.JScrollPane jScrollPane19;
    private javax.swing.JScrollPane jScrollPane20;
    private javax.swing.JScrollPane jScrollPane21;
    private javax.swing.JScrollPane jScrollPane22;
    private javax.swing.JScrollPane jScrollPane23;
    private javax.swing.JScrollPane jScrollPane24;
    private javax.swing.JScrollPane jScrollPane25;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane32;
    private javax.swing.JScrollPane jScrollPane6;
    private javax.swing.JScrollPane jScrollPane7;
    private javax.swing.JScrollPane jScrollPane8;
    private javax.swing.JScrollPane jScrollPane9;
    private javax.swing.JSeparator jSeparator15;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JTabbedPane jTabbedPane12;
    private javax.swing.JTabbedPane jTabbedPane14;
    private javax.swing.JTabbedPane jTabbedPane15;
    private javax.swing.JTabbedPane jTabbedPane16;
    private javax.swing.JTable jTable2;
    private javax.swing.JTable jTable3;
    private javax.swing.JTable jTable8;
    private javax.swing.JFormattedTextField jtfPDEC;
    private javax.swing.JFormattedTextField jtfPDF;
    private javax.swing.JFormattedTextField jtfPVP;
    private javax.swing.JComboBox mainWindowRepProf;
    private javax.swing.JTable memberList;
    private javax.swing.JButton nextApplication;
    private javax.swing.JTable participantsTable;
    private javax.swing.JTable paymentPlan;
    private javax.swing.JTextArea paymentPlanNotes;
    private javax.swing.JButton previousApplication;
    private javax.swing.JFormattedTextField projectBudget;
    private javax.swing.JFormattedTextField projectEta;
    private javax.swing.JFormattedTextField projectFinalCost;
    private javax.swing.JTable projectList;
    private javax.swing.JTextField projectName;
    private javax.swing.JTextArea projectNotes;
    private javax.swing.JFormattedTextField projectSignDate;
    private javax.swing.JFormattedTextField projectStartDate;
    private javax.swing.JButton removeDonationButton;
    private javax.swing.JButton removeDonorButton;
    private javax.swing.JButton removeEventButton;
    private javax.swing.JButton removeMember;
    private javax.swing.JButton removeTask;
    private javax.swing.JFormattedTextField repBirthDate;
    private javax.swing.JTextField repBirthPlace;
    private javax.swing.JTable repContacts;
    private javax.swing.JTextField repEducation;
    private javax.swing.JComboBox repMaritalStatus;
    private javax.swing.JComboBox repNationality;
    private javax.swing.JTextField repNib;
    private javax.swing.JTextField repNif;
    private javax.swing.JTextField search;
    private javax.swing.JButton submitDonorEdit;
    private javax.swing.JButton submitEditEventButton;
    private javax.swing.JButton submitEditFamily;
    private javax.swing.JButton submitEditProject;
    private javax.swing.JButton submitEditVolunteer;
    private javax.swing.JButton submitMembers;
    private javax.swing.JTable taskList;
    private javax.swing.JButton taskViewDetails;
    private javax.swing.JTextField volunteerAddress;
    private javax.swing.JFormattedTextField volunteerBirthDate;
    private javax.swing.JTextField volunteerBirthPlace;
    private javax.swing.JTextField volunteerEducation;
    private javax.swing.JTable volunteerEventList;
    private javax.swing.JComboBox volunteerMaritalStatus;
    private javax.swing.JTextField volunteerName;
    private javax.swing.JComboBox volunteerNationality;
    private javax.swing.JTextField volunteerNib;
    private javax.swing.JTextField volunteerNif;
    private javax.swing.JTextArea volunteerNotes;
    private javax.swing.JComboBox volunteerTeam;
    private javax.swing.JButton volunteerViewEventDetails;
    private javax.swing.JButton volunteerViewProjectDetails;
    // End of variables declaration//GEN-END:variables
    private final HashMap<String, JComponent> familyBackup = new HashMap<>();

}
