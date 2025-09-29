# 👨‍💼 Guida Amministratore - SAP Fiori Global Notification Banner

**Per amministratori di sistema che gestiscono le notifiche**

---

## 📋 Indice
1. [Panoramica](#panoramica)
2. [Accesso all'Interfaccia Admin](#accesso-allinterfaccia-admin)
3. [Creare una Notifica](#creare-una-notifica)
4. [Modificare una Notifica](#modificare-una-notifica)
5. [Eliminare una Notifica](#eliminare-una-notifica)
6. [Gestione Notifiche Multiple](#gestione-notifiche-multiple)
7. [Best Practices](#best-practices)
8. [Monitoraggio e Statistiche](#monitoraggio-e-statistiche)
9. [Risoluzione Problemi](#risoluzione-problemi)
10. [API Reference](#api-reference)

---

## 🎯 Panoramica

Come amministratore, hai la responsabilità di:
- ✅ Creare notifiche per tutti gli utenti
- ✅ Gestire priorità e validità temporale
- ✅ Monitorare notifiche attive
- ✅ Garantire comunicazioni efficaci

### Autorizzazioni Richieste
- **Ruolo SAP**: `Z_NOTIFICATION_ADMIN`
- **Oggetto Autorizzazione**: `Z_NOTIFY`
- **Attività**: Create (01), Change (02), Display (03), Delete (06)

---

## 🔐 Accesso all'Interfaccia Admin

### Metodo 1: Via Fiori Launchpad
1. Accedi al **Fiori Launchpad**
2. Cerca il tile **"Notification Administration"**
3. Clicca per aprire l'interfaccia admin

### Metodo 2: URL Diretto
```
https://your-s4hana-system.com/sap/bc/ui5_ui5/sap/z_notification_admin/index.html
```

### Dashboard Iniziale
Vedrai subito:
- 📊 **Statistiche**: Notifiche attive, per priorità, totali
- 📋 **Tabella**: Lista di tutte le notifiche
- 🔍 **Ricerca**: Campo per filtrare notifiche
- ➕ **Pulsanti**: Crea nuova, Aggiorna

```
┌─────────────────────────────────────────────────────────────┐
│  Notification Administration                    [➕ Create] [🔄 Refresh] │
├─────────────────────────────────────────────────────────────┤
│  Active: 5    High Priority: 2    Total: 12                │
├─────────────────────────────────────────────────────────────┤
│  Priority │ Title          │ Type │ Start    │ End      │ Status   │ Actions │
│  ─────────┼────────────────┼──────┼──────────┼──────────┼──────────┼─────────│
│  🔴 HIGH  │ System Down    │ URGENT│ 01/01/24 │ 01/02/24 │ Active   │ 👁️✏️🗑️  │
│  🟡 MEDIUM│ Maintenance    │ INFO │ 01/01/24 │ 31/12/24 │ Active   │ 👁️✏️🗑️  │
└─────────────────────────────────────────────────────────────┘
```

---

## ➕ Creare una Notifica

### Passo 1: Aprire il Dialog di Creazione
1. Clicca il pulsante **"Create Notification"** in alto a destra
2. Si apre un dialog con form vuoto

### Passo 2: Compilare i Campi

#### 📝 **Campi Obbligatori**

| Campo | Descrizione | Esempio |
|-------|-------------|---------|
| **Title** | Titolo breve e chiaro | "Sistema in Manutenzione" |
| **Message Text** | Descrizione dettagliata | "Il sistema sarà offline dalle 22:00 alle 24:00" |
| **Message Type** | Categoria del messaggio | URGENT, INFO, WARNING, MAINTENANCE |
| **Severity** | Livello di priorità | HIGH, MEDIUM, LOW |
| **Start Date** | Data inizio validità | 15/01/2024 |
| **End Date** | Data fine validità | 16/01/2024 |

#### 🎯 **Campi Opzionali**

| Campo | Descrizione | Valori | Default |
|-------|-------------|--------|---------|
| **Target Users** | Destinatari | ALL, SPECIFIC, ROLE_NAME | ALL |
| **Active** | Stato attivazione | X (attivo), vuoto (inattivo) | X |

### Passo 3: Scegliere la Priorità Corretta

#### 🔴 **HIGH (Alta) - Usare quando:**
- Sistema completamente offline
- Problemi di sicurezza critici
- Perdita di dati possibile
- Azione immediata richiesta
- **Esempio**: *"URGENTE: Sistema offline dalle 14:00. Salvare lavoro."*

#### 🟡 **MEDIUM (Media) - Usare quando:**
- Manutenzioni programmate
- Nuove funzionalità importanti
- Cambiamenti di processo
- Deadline imminenti
- **Esempio**: *"Manutenzione programmata domenica 20:00-22:00"*

#### 🔵 **LOW (Bassa) - Usare quando:**
- Informazioni generali
- Suggerimenti e tips
- Aggiornamenti non urgenti
- Comunicazioni generali
- **Esempio**: *"Nuova guida utente disponibile nella knowledge base"*

### Passo 4: Impostare Date Corrette

#### ⚠️ **Regole Importanti**
- **End Date** DEVE essere successiva a **Start Date**
- Le notifiche appaiono SOLO nel periodo Start-End
- Timezone: Usa il fuso orario del sistema SAP
- **Suggerimento**: Per emergenze, usa Start Date = oggi

#### 📅 **Esempi di Pianificazione**

**Emergenza Immediata:**
```
Start Date: 29/09/2024 (oggi)
End Date: 29/09/2024 (oggi)
Durata: Fino a fine giornata
```

**Manutenzione Programmata:**
```
Start Date: 01/10/2024 (5 giorni prima)
End Date: 07/10/2024 (giorno dopo manutenzione)
Durata: 7 giorni di preavviso
```

**Comunicazione Permanente:**
```
Start Date: 01/01/2024
End Date: 31/12/2024
Durata: Tutto l'anno
```

### Passo 5: Salvare e Verificare

1. Clicca **"Save"**
2. Sistema valida i dati:
   - ✅ Titolo non vuoto
   - ✅ Messaggio non vuoto
   - ✅ End Date > Start Date
3. Se OK: notifica creata e attiva
4. Se ERRORE: messaggio di errore specifico

#### Verifica Immediata
- La notifica appare nella tabella
- Status = "Active" (se dentro il periodo)
- Gli utenti la vedono entro 30 secondi

---

## ✏️ Modificare una Notifica

### Metodo 1: Dalla Tabella
1. Trova la notifica nella lista
2. Clicca l'icona **"Edit" (✏️)** nella colonna Actions
3. Si apre il dialog pre-compilato

### Metodo 2: Click sulla Riga
1. Clicca direttamente sulla riga della notifica
2. Si apre il dialog in modalità visualizzazione
3. Modifica i campi necessari

### Campi Modificabili
- ✅ **Title**: Aggiorna titolo
- ✅ **Message Text**: Modifica descrizione
- ✅ **Severity**: Cambia priorità
- ✅ **End Date**: Prolunga o accorcia validità
- ✅ **Active**: Attiva/disattiva
- ❌ **Message ID**: NON modificabile (generato dal sistema)
- ❌ **Start Date**: Modificabile con cautela

### Salvare le Modifiche
1. Clicca **"Save"**
2. Le modifiche sono immediate
3. Gli utenti vedono la versione aggiornata al prossimo refresh (max 30s)

---

## 🗑️ Eliminare una Notifica

### ⚠️ ATTENZIONE
L'eliminazione è **PERMANENTE** e **NON REVERSIBILE**!

### Procedura
1. Trova la notifica nella tabella
2. Clicca l'icona **"Delete" (🗑️)**
3. Conferma nel dialog: "Are you sure you want to delete this notification?"
4. Clicca **"OK"** per confermare o **"Cancel"** per annullare

### Cosa Succede
- ✅ Notifica rimossa dal database
- ✅ Sparisce dalla tabella admin
- ✅ Non più visibile agli utenti (entro 30s)
- ❌ **NON recuperabile** - backup necessario se importante

### Alternative all'Eliminazione

#### Opzione 1: Disattivazione
Invece di eliminare, puoi **disattivare**:
1. Modifica la notifica
2. Togli la spunta da **"Active"**
3. Salva
4. **Risultato**: Notifica nascosta ma recuperabile

#### Opzione 2: Riduzione Validità
Accorcia la validità:
1. Modifica la notifica
2. Imposta **End Date** = oggi
3. Salva
4. **Risultato**: Notifica scade automaticamente

---

## 📊 Gestione Notifiche Multiple

### Priorità di Visualizzazione
Quando ci sono più notifiche attive:
1. **HIGH** appaiono per prime
2. **MEDIUM** in seconda posizione
3. **LOW** per ultime

### Navigazione Utente
Gli utenti vedranno:
```
⬅️ [1 di 5] ➡️
```
E possono navigare tra tutte le notifiche attive.

### Best Practice per Multiple Notifiche
- ✅ Massimo **3-5 notifiche** attive contemporaneamente
- ✅ Solo **1 notifica HIGH** alla volta (se possibile)
- ✅ Raggruppa informazioni simili in una sola notifica
- ❌ Evita sovraccarico di informazioni

### Esempio di Gestione Efficace

**❌ NON FARE:**
```
1. HIGH: Sistema offline
2. HIGH: Backup in corso
3. HIGH: Database non disponibile
4. HIGH: Servizio email down
5. HIGH: Report non funzionanti
```
*Troppo! Gli utenti sono confusi.*

**✅ FARE:**
```
1. HIGH: Sistema in Manutenzione
   "Molteplici servizi temporaneamente non disponibili
    dalle 22:00 alle 24:00. Backup, email e report
    torneranno operativi dopo la manutenzione."
```
*Chiaro e conciso!*

---

## 💡 Best Practices

### Scrittura Messaggi Efficaci

#### ✅ **DO - Fare**
- **Chiaro e conciso**: Max 2-3 frasi
- **Azione specifica**: "Salvare il lavoro entro le 14:00"
- **Tempistiche precise**: "dalle 22:00 alle 24:00" non "stasera"
- **Informazioni complete**: Chi, cosa, quando, perché
- **Tono professionale**: Evita abbreviazioni, emoji, CAPS LOCK

#### ❌ **DON'T - Non Fare**
- Messaggi vaghi: "Problemi di sistema" (quali?)
- Troppo lunghi: Più di 200 caratteri
- Allarmismo: "EMERGENZA!!!" (a meno che non lo sia)
- Informazioni mancanti: "Domani manutenzione" (che ora?)
- Errori grammaticali: Rileggi sempre

### Esempi Buoni vs Cattivi

**❌ Cattivo:**
```
"ATTENZIONE!!!! Il sistema avrà dei problemi.
Potreste non riuscire a lavorare. Scusate."
```
*Vago, allarmista, senza dettagli*

**✅ Buono:**
```
"Manutenzione Programmata: Sistema SAP non disponibile
domenica 15/10 dalle 20:00 alle 22:00. Salvare il lavoro
prima delle 19:45."
```
*Chiaro, preciso, con azione richiesta*

### Pianificazione Temporale

#### Per Manutenzioni Programmate
- **5-7 giorni prima**: Notifica LOW con preavviso
- **24 ore prima**: Notifica MEDIUM con reminder
- **1 ora prima**: Notifica HIGH con countdown
- **Durante**: Notifica HIGH con status

#### Per Emergenze
- **Immediata**: Notifica HIGH non appena identificato il problema
- **Aggiornamenti**: Modifica la stessa notifica con status update
- **Risoluzione**: Cambia a notifica LOW/SUCCESS con conferma

### Targeting Utenti

#### Target: ALL
- Usa per: Comunicazioni di sistema generale
- Esempio: Manutenzioni, downtime, nuove features

#### Target: SPECIFIC
- Usa per: Messaggi a utenti specifici
- Esempio: "Utenti del modulo Finance: nuovo report disponibile"

#### Target: ROLE_NAME
- Usa per: Comunicazioni per ruolo
- Esempio: "Amministratori: aggiornamento security da applicare"

---

## 📈 Monitoraggio e Statistiche

### Dashboard Header
Il dashboard mostra sempre:

```
┌─────────────────────────────────────────┐
│  Active: 5  │  High Priority: 2  │  Total: 12  │
└─────────────────────────────────────────┘
```

- **Active**: Notifiche attualmente visibili agli utenti
- **High Priority**: Numero di notifiche critiche
- **Total**: Tutte le notifiche nel sistema (anche inattive/scadute)

### Filtri e Ricerca

#### Campo Ricerca
Cerca in:
- Titolo notifica
- Testo messaggio
- Tipo messaggio

#### Filtri Automatici
- Ordina per priorità
- Ordina per data
- Filtra per status (Active/Inactive)

### Report e Analytics

#### Notifiche per Periodo
```sql
SELECT COUNT(*)
FROM ZTNOTIFY_MSGS
WHERE START_DATE >= '20240101'
  AND END_DATE <= '20241231'
```

#### Notifiche per Tipo
```sql
SELECT SEVERITY, COUNT(*)
FROM ZTNOTIFY_MSGS
GROUP BY SEVERITY
```

#### Notifiche Attive Ora
```sql
SELECT *
FROM ZT_NOTIFY_MESSAGES
WHERE ACTIVE = 'X'
```

---

## 🔧 Risoluzione Problemi

### Problema: Notifica Non Appare agli Utenti

#### Checklist Diagnostica
- [ ] Verifica **Active** = 'X'
- [ ] Controlla **Start Date** ≤ oggi
- [ ] Controlla **End Date** ≥ oggi
- [ ] Verifica autorizzazioni utente
- [ ] Controlla servizio REST attivo (SICF)
- [ ] Test API manualmente

#### Test API
```bash
curl -X GET "https://your-system/sap/bc/rest/zcl_notification_rest/" \
  -H "Authorization: Basic <credentials>"
```

**Risposta Attesa:**
```json
[
  {
    "message_id": "...",
    "title": "Test",
    "message_text": "Messaggio",
    "severity": "HIGH",
    "active": "X"
  }
]
```

### Problema: Errore Durante Salvataggio

#### Errori Comuni

**Errore**: "Title is required"
- **Causa**: Campo titolo vuoto
- **Soluzione**: Inserisci un titolo

**Errore**: "End date must be after start date"
- **Causa**: Date invertite
- **Soluzione**: End Date deve essere dopo Start Date

**Errore**: "Unauthorized"
- **Causa**: Mancano autorizzazioni
- **Soluzione**: Verifica ruolo Z_NOTIFICATION_ADMIN

### Problema: Notifica Duplicata

#### Soluzione
1. Identifica la notifica duplicata
2. Elimina quella con ID più recente
3. O disattiva una delle due

### Problema: Performance Lente

#### Cause Possibili
- Troppe notifiche attive (>10)
- Database non ottimizzato
- Polling troppo frequente

#### Soluzioni
1. Archivia notifiche vecchie (>90 giorni)
2. Crea indici su ZTNOTIFY_MSGS:
   - INDEX su (START_DATE, END_DATE)
   - INDEX su (ACTIVE)
3. Considera aumento polling interval a 60s

---

## 🔌 API Reference

### REST Endpoints

#### GET - Recupera Notifiche
```http
GET /sap/bc/rest/zcl_notification_rest/
```

**Query Parameters:**
- `user_id` (optional): Filtra per utente specifico

**Response:**
```json
[
  {
    "message_id": "550e8400-e29b-41d4-a716-446655440001",
    "message_type": "URGENT",
    "severity": "HIGH",
    "title": "System Maintenance",
    "message_text": "System will be down...",
    "start_date": "20240115",
    "end_date": "20240116",
    "target_users": "ALL",
    "active": "X",
    "created_by": "ADMIN",
    "created_at": "20240115120000",
    "changed_by": "ADMIN",
    "changed_at": "20240115120000"
  }
]
```

#### POST - Crea Notifica
```http
POST /sap/bc/rest/zcl_notification_rest/
Content-Type: application/json
X-CSRF-Token: <token>
```

**Request Body:**
```json
{
  "message_type": "URGENT",
  "severity": "HIGH",
  "title": "System Maintenance",
  "message_text": "System will be unavailable from 10 PM to 12 AM",
  "start_date": "20240115",
  "end_date": "20240116",
  "target_users": "ALL",
  "active": "X"
}
```

**Response:**
```json
{
  "message_id": "550e8400-e29b-41d4-a716-446655440001",
  "status": "success"
}
```

#### PUT - Aggiorna Notifica
```http
PUT /sap/bc/rest/zcl_notification_rest/?message_id=<id>
Content-Type: application/json
X-CSRF-Token: <token>
```

**Request Body:** (Campi da modificare)
```json
{
  "title": "Updated Title",
  "end_date": "20240120"
}
```

#### DELETE - Elimina Notifica
```http
DELETE /sap/bc/rest/zcl_notification_rest/?message_id=<id>
X-CSRF-Token: <token>
```

**Response:**
```json
{
  "status": "deleted",
  "message_id": "550e8400-e29b-41d4-a716-446655440001"
}
```

### CSRF Token
Prima di POST/PUT/DELETE, recupera token:
```http
GET /sap/bc/rest/zcl_notification_rest/
X-CSRF-Token: Fetch
```

Response Header conterrà: `X-CSRF-Token: <actual-token>`

---

## 📋 Checklist Amministratore

### Setup Iniziale
- [ ] Verifica autorizzazioni (Z_NOTIFICATION_ADMIN)
- [ ] Test accesso interfaccia admin
- [ ] Verifica connessione API REST
- [ ] Test creazione notifica di prova
- [ ] Elimina notifica di prova

### Operazioni Quotidiane
- [ ] Controlla notifiche scadute (rimuovere?)
- [ ] Verifica notifiche attive (massimo 5)
- [ ] Leggi feedback utenti
- [ ] Aggiorna notifiche se necessario

### Operazioni Settimanali
- [ ] Pulisci notifiche vecchie (>30 giorni)
- [ ] Rivedi priorità notifiche attive
- [ ] Controlla performance sistema
- [ ] Backup notifiche importanti

### Operazioni Mensili
- [ ] Archivia notifiche >90 giorni
- [ ] Report utilizzo e statistiche
- [ ] Review best practices con team
- [ ] Aggiorna documentazione se necessario

---

## 📞 Supporto Tecnico

### Contatti
- **Sviluppo**: [Gabriele Rendina - gabriele.rendina@lutech.it](mailto:Gabriele Rendina - gabriele.rendina@lutech.it)
- **Basis SAP**: [basis-team@lutech.it](mailto:basis-team@lutech.it)
- **Security**: [security@lutech.it](mailto:security@lutech.it)

### Escalation
1. **Livello 1**: Help Desk interno
2. **Livello 2**: Team sviluppo SAP
3. **Livello 3**: SAP Basis / consultants esterni

---

## 📚 Risorse Aggiuntive

- [📖 Deployment Guide](DEPLOYMENT_GUIDE.md) - Setup tecnico completo
- [👤 User Guide](USER_GUIDE.md) - Guida per utenti finali
- [🔍 API Documentation](#api-reference) - Reference API REST
- [📊 Test Report](../TEST_REPORT.md) - Report test di sistema

---

**💼 Sei un amministratore responsabile. Usa questo strumento con saggezza per comunicare efficacemente con tutti gli utenti!**