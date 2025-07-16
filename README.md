<div align="center">
  <h1>Juspay-like Payment Orchestration Backend</h1>
  <p><em>Inspired by Juspay, a leading fintech company</em></p>
</div>

---

## Overview

This is a simple, production-grade backend for online payments, designed for Indian merchants and inspired by Juspay. Built with free, open-source tools, it handles payments (UPI, cards), fraud checks, automatic retries, and performance tracking. The project is easy to set up, runs anywhere (Docker/Render), and is licensed under MIT for public use.


## What Does This Project Do?

Imagine you’re buying something online and paying with UPI or a credit card. This backend works behind the scenes to make your payment fast and safe. It:

- **Takes your payment request** (e.g., “Pay ₹15,000”)
- **Picks the best way to process it** (chooses a reliable cashier)
- **Checks if the payment is safe** (blocks suspicious transactions)
- **Retries if something goes wrong**
- **Keeps payment details secure** and tracks all activity

> **Performance:** Handles up to **500 payments/second** (for learning/small business; Juspay handles 20,000+)

---

## Features

- **Start Payments:** UPI, cards, netbanking via Instamojo (free for merchants)
- **Smart Choices:** Picks best payment service based on amount (premium for > ₹10,000)
- **Fraud Protection:** Blocks suspicious payments (e.g., > ₹1,00,000 or unusual locations)
- **Automatic Retries:** Retries failed payments
- **Secure Storage:** Stores payment details securely in PostgreSQL
- **Real-Time Updates:** Instant notifications for payment status
- **Performance Tracking:** Prometheus metrics for success rate and speed

---

## Tools Used

| Tool         | Purpose                                      |
| ------------ | -------------------------------------------- |
| Haskell      | Type-safe backend logic                      |
| Instamojo    | Payment gateway (India)                      |
| PostgreSQL   | Database for payment records                 |
| Kafka        | Messaging for retries/updates                |
| Redis        | Fast cache for processor health/status       |
| Prometheus   | Performance monitoring                       |
| Docker       | Containerization for portability             |
| Render       | Cloud hosting                                |
| GitHub       | Public code sharing                          |

---

## Setup Instructions

### 1. Install Prerequisites

Install these free tools:

```bash
# Haskell (Stack)
curl -sSL https://get.haskellstack.org/ | sh

# PostgreSQL
sudo apt install postgresql

# Kafka
# Download and unzip from https://kafka.apache.org

# Redis
sudo apt install redis-server

# Prometheus
# Download and unzip from https://prometheus.io

# ngrok
sudo snap install ngrok

# Docker
sudo apt install docker.io

# Git
sudo apt install git
```

Sign up for an **Instamojo** account at [instamojo.com](https://www.instamojo.com), enable convenience fees, and get test API keys.

### 2. Clone the Project

```bash
git clone https://github.com/harisai-karanam/juspay-backend.git
cd juspay-backend
```

### 3. Configure Environment

Create a `.env` file:

```bash
echo -e "INSTAMOJO_API_KEY=your_api_key\nINSTAMOJO_AUTH_TOKEN=your_auth_token\nINSTAMOJO_WEBHOOK_SECRET=your_webhook_secret\nPOSTGRES_URL=postgresql://juspay_user:password@localhost:5432/juspay\nKAFKA_BOOTSTRAP=localhost:9092\nREDIS_URL=redis://localhost:6379" > .env
chmod 600 .env
```
Replace placeholders with your Instamojo test credentials.

### 4. Set Up Database

Start PostgreSQL:
```bash
sudo systemctl start postgresql
```
Create user and database:
```bash
psql -U postgres -c "CREATE USER juspay_user WITH PASSWORD 'password';"
psql -U postgres -c "CREATE DATABASE juspay OWNER juspay_user;"
```
Set up tables:
```bash
psql -h localhost -U juspay_user -d juspay
```
Run in psql prompt:
```sql
CREATE TABLE transaction (
    id VARCHAR PRIMARY KEY,
    amount INTEGER,
    currency VARCHAR,
    status VARCHAR,
    processor VARCHAR,
    token VARCHAR,
    ip_address VARCHAR,
    fraud_flag BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
CREATE TABLE audit_log (
    id SERIAL PRIMARY KEY,
    transaction_id VARCHAR,
    event VARCHAR,
    details JSONB,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
\q
```

### 5. Start Supporting Services

Start Kafka:
```bash
/usr/local/kafka/bin/zookeeper-server-start.sh /usr/local/kafka/config/zookeeper.properties &
/usr/local/kafka/bin/kafka-server-start.sh /usr/local/kafka/config/server.properties &
```
Start Redis:
```bash
redis-server &
```
Start Prometheus:
```bash
/usr/local/prometheus/prometheus --config.file=/usr/local/prometheus/prometheus.yml &
```
Create `prometheus.yml`:
```yaml
global:
  scrape_interval: 15s
scrape_configs:
  - job_name: 'juspay-backend'
    static_configs:
      - targets: ['localhost:8080']
```
Set Redis health flags:
```bash
redis-cli set processor:instamojo_premium:health healthy
redis-cli set processor:instamojo_standard:health healthy
```

### 6. Build and Run

Install dependencies:
```bash
stack install
```
Run the backend:
```bash
stack run juspay-backend
```

---

## Testing the System

### Start a Payment
```bash
curl -X POST http://localhost:8080/create-payment \
-H "Content-Type: application/json" \
-d '{"amount": 15000, "currency": "INR", "cardDetails": {}, "ipAddress": "192.168.1.1", "country": "IN"}'
```
You should get a payment link from Instamojo.

### Check for Fraud
Test a large payment:
```bash
curl -X POST http://localhost:8080/create-payment \
-H "Content-Type: application/json" \
-d '{"amount": 150000, "currency": "INR", "cardDetails": {}, "ipAddress": "192.168.1.1", "country": "IN"}'
```
Should return: `Fraud detected: High amount.`

### Test Updates
Start ngrok:
```bash
ngrok http 8080
```
Update Instamojo’s webhook URL to `https://<ngrok-id>.ngrok.io/webhook`.
Send a test update:
```bash
curl -X POST http://localhost:8080/webhook \
-H "Content-Type: application/json" \
-d '{"payment_request_id": "12345", "status": "success"}'
```

### Check Performance
Visit [http://localhost:9090](http://localhost:9090) for Prometheus metrics.

### Run Unit Tests
```bash
stack test
```

---

## Deploying Online (Render)

1. **Push to GitHub:**
   ```bash
   git add .
   git commit -m "Ready for deployment"
   git push origin main
   ```
2. **Create a Web Service** on [Render](https://dashboard.render.com):
   - Runtime: Docker
   - Build Command: `stack install`
   - Start Command: `stack exec juspay-backend`
   - Set environment variables:
     - `INSTAMOJO_API_KEY=your_api_key`
     - `INSTAMOJO_AUTH_TOKEN=your_auth_token`
     - `INSTAMOJO_WEBHOOK_SECRET=your_webhook_secret`
     - `POSTGRES_URL=postgresql://<render-postgres-url>`
     - `KAFKA_BOOTSTRAP=<render-kafka-url>`
     - `REDIS_URL=redis://<render-redis-url>`
   - Create Render’s PostgreSQL, Kafka, and Redis services, and copy their URLs.
3. **Test:**
   ```bash
   curl https://juspay-backend.onrender.com/create-payment ...
   ```
   Update Instamojo’s webhook URL to `https://juspay-backend.onrender.com/webhook`.

---

## What’s Next?

- Add more payment services (PayU, Cashfree, etc.)
- Build a dashboard/webpage for payment data
- Improve fraud detection and processing speed

---

## License

This project uses the MIT License. You are free to use or modify it, as long as you credit me (Harisai Karanam). See [LICENSE](./LICENSE) for details.



## Thanks

Inspired by Juspay’s amazing work and built with free tools like Haskell, Instamojo, and Docker. Thanks to the open-source community for making this possible!
