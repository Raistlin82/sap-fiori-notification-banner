# 👤 Guida Utente - SAP Fiori Global Notification Banner

**Per utenti finali delle applicazioni SAP Fiori**

---

## 📋 Indice
1. [Introduzione](#introduzione)
2. [Come Funziona](#come-funziona)
3. [Tipi di Notifiche](#tipi-di-notifiche)
4. [Interazioni Utente](#interazioni-utente)
5. [Dispositivi Supportati](#dispositivi-supportati)
6. [Accessibilità](#accessibilità)
7. [FAQ](#faq)

---

## 🎯 Introduzione

Il **Global Notification Banner** è un sistema che mostra automaticamente messaggi importanti in cima a tutte le applicazioni SAP Fiori che utilizzi. Non devi installare nulla - il banner appare automaticamente quando ci sono notifiche attive.

### Cosa Vedrai
- Banner colorati con messaggi importanti
- Icone che indicano il tipo di messaggio
- Pulsanti per navigare e chiudere
- Design responsive su tutti i dispositivi

---

## 🔄 Come Funziona

### Apparizione Automatica
Il banner appare automaticamente quando:
- Accedi a qualsiasi applicazione Fiori
- Vengono pubblicate nuove notifiche
- Il sistema trova messaggi attivi per il tuo utente

### Aggiornamento in Tempo Reale
- Il sistema controlla ogni **30 secondi** se ci sono nuove notifiche
- Le notifiche scadute scompaiono automaticamente
- Non devi ricaricare la pagina

---

## 🎨 Tipi di Notifiche

### 🔴 **Priorità Alta (Critica)**
- **Colore**: Rosso con bordo rosso scuro
- **Utilizzo**: Problemi critici, emergenze, system down
- **Esempio**: *"Sistema in manutenzione straordinaria dalle 14:00 alle 16:00"*
- **Azione**: Leggi subito e prendi nota

### 🟡 **Priorità Media (Avviso)**
- **Colore**: Giallo/Arancione
- **Utilizzo**: Avvisi importanti, modifiche programmate
- **Esempio**: *"Nuova funzionalità disponibile da lunedì prossimo"*
- **Azione**: Leggi quando possibile

### 🔵 **Priorità Bassa (Informativa)**
- **Colore**: Blu
- **Utilizzo**: Informazioni generali, suggerimenti
- **Esempio**: *"Ricorda di aggiornare i tuoi dati di profilo"*
- **Azione**: Leggi quando hai tempo

### 🟢 **Successo**
- **Colore**: Verde
- **Utilizzo**: Conferme positive, completamenti
- **Esempio**: *"Migrazione dati completata con successo"*

### ⚪ **Manutenzione**
- **Colore**: Grigio
- **Utilizzo**: Manutenzioni programmate
- **Esempio**: *"Manutenzione pianificata domenica notte"*

---

## 🖱️ Interazioni Utente

### Visualizzazione Base
```
┌─────────────────────────────────────────────────────────┐
│ ⚠️ TITOLO NOTIFICA: Testo del messaggio importante     ❌ │
└─────────────────────────────────────────────────────────┘
```

### Con Notifiche Multiple
```
┌─────────────────────────────────────────────────────────┐
│ ⚠️ TITOLO: Messaggio    ⬅️ [1 di 3] ➡️              ❌ │
└─────────────────────────────────────────────────────────┘
```

### Azioni Disponibili

#### ❌ **Chiudi Notifica**
- **Cosa fa**: Rimuove la notifica corrente dalla visualizzazione
- **Come**: Clicca sulla **X** a destra
- **Nota**: La notifica non verrà più mostrata a te, ma resta attiva per altri utenti

#### ⬅️ **Notifica Precedente**
- **Cosa fa**: Mostra la notifica precedente
- **Come**: Clicca sulla **freccia sinistra**
- **Disponibile**: Solo se ci sono più notifiche

#### ➡️ **Notifica Successiva**
- **Cosa fa**: Mostra la notifica successiva
- **Come**: Clicca sulla **freccia destra**
- **Disponibile**: Solo se ci sono più notifiche

#### 📊 **Contatore**
- **Cosa mostra**: Posizione corrente (es. "1 di 3")
- **Utilità**: Capire quante notifiche totali ci sono

---

## 📱 Dispositivi Supportati

### 🖥️ **Desktop**
- **Visualizzazione**: Banner completo con tutti i controlli
- **Posizione**: Fisso in cima alla pagina
- **Interazione**: Click con mouse

### 📱 **Tablet**
- **Visualizzazione**: Layout adattivo, controlli essenziali
- **Posizione**: Fisso in cima, si adatta alla larghezza
- **Interazione**: Touch ottimizzato

### 📱 **Smartphone**
- **Visualizzazione**: Compatto, testo su più righe se necessario
- **Controlli**: Semplificati (nasconde contatore su schermi piccoli)
- **Interazione**: Touch friendly con target più grandi

### Browser Supportati
- ✅ **Chrome 90+**
- ✅ **Firefox 88+**
- ✅ **Safari 14+**
- ✅ **Edge 90+**
- ⚠️ **Internet Explorer 11** (supporto limitato)

---

## ♿ Accessibilità

### Screen Reader
- **Supporto completo** per NVDA, JAWS, VoiceOver
- **Annunci automatici** quando appaiono nuove notifiche
- **Navigazione tastiera** completa

### Navigazione da Tastiera
- **Tab**: Naviga tra i controlli
- **Enter/Space**: Attiva pulsanti
- **Escape**: Chiude la notifica
- **Frecce**: Naviga tra notifiche multiple

### Alto Contrasto
- **Supporto automatico** per modalità alto contrasto del sistema
- **Bordi più spessi** e colori contrastanti
- **Compatibile** con impostazioni di accessibilità Windows/Mac

### Riduzione Movimento
- **Animazioni disabilitate** se impostato nel sistema
- **Transizioni semplificate** per utenti sensibili al movimento

---

## ❓ FAQ - Domande Frequenti

### **Q: Il banner non appare, cosa faccio?**
**A**: Verifica che:
- Sei loggato correttamente in SAP
- Il tuo browser sia supportato
- Non ci siano blocker di popup attivi
- Contatta l'amministratore se il problema persiste

### **Q: Posso disattivare le notifiche?**
**A**: No, le notifiche sono gestite centralmente dagli amministratori per garantire che informazioni critiche raggiungano tutti gli utenti.

### **Q: Ho chiuso una notifica per errore, come la rivedo?**
**A**: Una volta chiusa, la notifica non riapparirà per il tuo utente. Contatta l'amministratore se hai bisogno di rivedere il contenuto.

### **Q: Perché alcune notifiche hanno priorità diversa?**
**A**: Le priorità aiutano a distinguere:
- **Alta**: Azioni immediate richieste
- **Media**: Informazioni importanti
- **Bassa**: Informazioni generali

### **Q: Le notifiche funzionano su tutte le app Fiori?**
**A**: Sì, il banner appare automaticamente su tutte le applicazioni Fiori del sistema.

### **Q: Cosa succede se sono offline?**
**A**: Le notifiche vengono caricate quando torni online. Il sistema verifica automaticamente ogni 30 secondi.

### **Q: Posso vedere notifiche vecchie?**
**A**: No, vengono mostrate solo le notifiche attive nel periodo corrente. Le notifiche scadute non sono più visibili.

### **Q: Il banner copre contenuto importante, cosa faccio?**
**A**: Il banner è progettato per non interferire, ma puoi:
- Chiudere la notifica se non più rilevante
- Scrollare la pagina per vedere il contenuto sottostante

### **Q: Su mobile il banner è diverso?**
**A**: Sì, è ottimizzato per schermi piccoli:
- Layout più compatto
- Controlli touch-friendly
- Testo che si adatta alla larghezza

### **Q: Posso segnalare problemi con una notifica?**
**A**: Contatta direttamente l'amministratore del sistema o il team IT per problemi con il contenuto o il funzionamento.

---

## 📞 Supporto

### Contatti
- **Team IT**: [Gabriele Rendina - gabriele.rendina@lutech.it](mailto:Gabriele Rendina - gabriele.rendina@lutech.it)
- **Help Desk**: [Ileana Scaglia - ileana.scaglia@lutech.it](mailto:Ileana Scaglia - ileana.scaglia@lutech.it)
- **Telefono**: +39 XXX XXX XXXX

### Segnalazioni
Per problemi tecnici, includi sempre:
- Browser utilizzato e versione
- Dispositivo (desktop/mobile/tablet)
- Screenshot se possibile
- Ora e data del problema

---

**📱 Per una migliore esperienza, tieni il browser aggiornato e abilita JavaScript.**