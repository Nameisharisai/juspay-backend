Juspay-like Payment Orchestration Backend
This is a simple system that processes online payments for Indian merchants, inspired by Juspay, a leading fintech company. Built with free tools, it handles payments (like UPI or cards), checks for fraud, retries failed payments, and tracks performance. Whether you’re a student, developer, or just curious, this project is easy to set up and use! It’s hosted on GitHub and runs online with Render, using an MIT License so anyone can use it.
This README explains the project in plain English, guiding you to set it up, test it, and even make it live online. For a detailed 10+ page report, check out project_report.pdf (upload to Google Drive/Dropbox and add link).
What Does This Project Do?
Imagine you’re buying something online and paying with UPI or a credit card. This system works behind the scenes to make that payment fast and safe. It’s like a manager who:

Takes your payment request (e.g., “Pay ₹15,000”).
Picks the best way to process it (like choosing a reliable cashier).
Checks if the payment is safe (e.g., blocks suspicious transactions).
Retries if something goes wrong.
Keeps payment details secure and tracks how everything is working.

It’s built to handle up to 500 payments per second, much smaller than Juspay’s 20,000, but perfect for learning and small businesses!
Features
Here’s what the system can do:

Start Payments: Process payments via UPI, cards, or netbanking using Instamojo (free for merchants via convenience fees).
Smart Choices: Picks the best payment service based on the amount (e.g., premium for payments over ₹10,000).
Fraud Protection: Stops suspicious payments, like those over ₹1,00,000 or from unusual locations.
Automatic Retries: Tries failed payments again to ensure they succeed.
Secure Storage: Keeps payment details safe in a database, following security rules.
Real-Time Updates: Gets instant notifications when payments succeed or fail.
Performance Tracking: Shows how many payments work and how fast they process.

Tools Used
The project uses free, easy-to-get tools:

Haskell: A programming language that prevents mistakes.
Instamojo: A free payment service for India.
PostgreSQL: A database to store payment records.
Kafka: Manages retries and updates, like a messaging system.
Redis: Stores temporary data, like which payment service is working.
Prometheus: Tracks performance, like a report card.
Docker: Packages the system to run anywhere.
Render: Hosts the system online.
GitHub: Shares the project for everyone to see.

How to Set It Up
Follow these steps to run the system on your computer (works on Ubuntu, Windows, or Mac).
Step 1: Install Tools
Download and install these free tools:

Haskell: Get it at haskell.org. Run curl -sSL https://get.haskellstack.org/ | sh.
PostgreSQL: Install with sudo apt install postgresql (Ubuntu) or from postgresql.org.
Kafka: Download from kafka.apache.org and unzip.
Redis: Install with sudo apt install redis-server (Ubuntu) or from redis.io.
Prometheus: Get it from prometheus.io and unzip.
ngrok: Install via sudo snap install ngrok or from ngrok.com.
Docker: Install with sudo apt install docker.io (Ubuntu) or from docker.com.
Git: Install with sudo apt install git or from git-scm.com.
Instamojo Account: Sign up at instamojo.com, enable convenience fees, and get test API keys.

Step 2: Get the Project
Copy the project to your computer:
git clone https://github.com/harisai-karanam/juspay-backend.git
cd juspay-backend

Step 3: Set Up Configuration
Create a file called .env to store settings:
echo -e "INSTAMOJO_API_KEY=your_api_key\nINSTAMOJO_AUTH_TOKEN=your_auth_token\nINSTAMOJO_WEBHOOK_SECRET=your_webhook_secret\nPOSTGRES_URL=postgresql://juspay_user:password@localhost:5432/juspay\nKAFKA_BOOTSTRAP=localhost:9092\nREDIS_URL=redis://localhost:6379" > .env
chmod 600 .env

Replace your_api_key, your_auth_token, and your_webhook_secret with your Instamojo test credentials.
Step 4: Set Up the Database
Start PostgreSQL:
sudo systemctl start postgresql

Create a database and user:
psql -U postgres -c "CREATE USER juspay_user WITH PASSWORD 'password';"
psql -U postgres -c "CREATE DATABASE juspay OWNER juspay_user;"

Set up tables:
psql -h localhost -U juspay_user -d juspay

Run these commands in the psql prompt:
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

Step 5: Start Other Tools
Run Kafka:
/usr/local/kafka/bin/zookeeper-server-start.sh /usr/local/kafka/config/zookeeper.properties &
/usr/local/kafka/bin/kafka-server-start.sh /usr/local/kafka/config/server.properties &

Start Redis: redis-server &Start Prometheus:
/usr/local/prometheus/prometheus --config.file=/usr/local/prometheus/prometheus.yml &

Create prometheus.yml in /usr/local/prometheus:
global:
  scrape_interval: 15s
scrape_configs:
  - job_name: 'juspay-backend'
    static_configs:
      - targets: ['localhost:8080']

Set Redis data:
redis-cli set processor:instamojo_premium:health healthy
redis-cli set processor:instamojo_standard:health healthy

Step 6: Install and Run
Install project dependencies:
stack install

Run the system:
stack run juspay-backend

Testing the System
Try these tests to make sure everything works:

Start a Payment:

curl -X POST http://localhost:8080/create-payment \
-H "Content-Type: application/json" \
-d '{"amount": 15000, "currency": "INR", "cardDetails": {}, "ipAddress": "192.168.1.1", "country": "IN"}'

You should get a payment link from Instamojo.

Check for Fraud:Test a large payment:

curl -X POST http://localhost:8080/create-payment \
-H "Content-Type: application/json" \
-d '{"amount": 150000, "currency": "INR", "cardDetails": {}, "ipAddress": "192.168.1.1", "country": "IN"}'

It should say “Fraud detected: High amount.”

Test Updates:Start ngrok: ngrok http 8080, copy the URL (e.g., https://abc123.ngrok.io), and update Instamojo’s webhook URL to https://abc123.ngrok.io/webhook.Send a test update:

curl -X POST http://localhost:8080/webhook \
-H "Content-Type: application/json" \
-d '{"payment_request_id": "12345", "status": "success"}'


Check Performance:Visit http://localhost:9090 to see Prometheus data, like how many payments succeeded.

Run Unit Tests:


stack test

This checks all parts, like fraud detection and payment routing.
Deploying Online
Make the system live on Render:

Push to GitHub:

git add .
git commit -m "Ready for deployment"
git push origin main


Go to dashboard.render.com, create a Web Service, and connect to juspay-backend.
Set:
Runtime: Docker
Build Command: stack install
Start Command: stack exec juspay-backend
Environment Variables:INSTAMOJO_API_KEY=your_api_key
INSTAMOJO_AUTH_TOKEN=your_auth_token
INSTAMOJO_WEBHOOK_SECRET=your_webhook_secret
POSTGRES_URL=postgresql://<render-postgres-url>
KAFKA_BOOTSTRAP=<render-kafka-url>
REDIS_URL=redis://<render-redis-url>




Create Render’s PostgreSQL, Kafka, and Redis services, and copy their URLs.
Test: curl https://juspay-backend.onrender.com/create-payment ...
Update Instamojo’s webhook URL to https://juspay-backend.onrender.com/webhook.

What’s Next?
You can improve the system by:

Adding more payment services like PayU or Cashfree.
Making a webpage to show payment data, like a dashboard.
Using smarter fraud detection or faster processing.

License
This project uses the MIT License, so you can use or modify it freely, as long as you credit me (Harisai Karanam). See LICENSE for details.
Learn More
Read my 10+ page report for a full story of how I built this: project_report.pdf (upload to Google Drive/Dropbox and add link).
Thanks
Inspired by Juspay’s amazing work and built with free tools like Haskell, Instamojo, and Docker. Thanks to the open-source community for making this possible!