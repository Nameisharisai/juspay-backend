DROP TABLE IF EXISTS audit_log CASCADE;
DROP TABLE IF EXISTS transaction CASCADE;

CREATE TABLE transaction (
    id VARCHAR(255) PRIMARY KEY,
    amount INTEGER NOT NULL CHECK (amount > 0),
    currency VARCHAR(3) NOT NULL CHECK (length(currency) = 3),
    status VARCHAR(50) NOT NULL DEFAULT 'pending',
    processor VARCHAR(100) NOT NULL,
    token VARCHAR(500),
    ip_address INET NOT NULL,
    fraud_flag BOOLEAN DEFAULT FALSE,
    fraud_score DOUBLE PRECISION DEFAULT 0.0,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE audit_log (
    id SERIAL PRIMARY KEY,
    transaction_id VARCHAR(255) REFERENCES transaction(id),
    event VARCHAR(100) NOT NULL,
    details JSONB,
    ip_address INET,
    user_agent TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_transaction_status ON transaction(status);
CREATE INDEX idx_transaction_created_at ON transaction(created_at);
CREATE INDEX idx_transaction_processor ON transaction(processor);
CREATE INDEX idx_transaction_fraud_flag ON transaction(fraud_flag);
CREATE INDEX idx_transaction_ip_address ON transaction(ip_address);

CREATE INDEX idx_audit_log_transaction_id ON audit_log(transaction_id);
CREATE INDEX idx_audit_log_event ON audit_log(event);
CREATE INDEX idx_audit_log_created_at ON audit_log(created_at);

CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER update_transaction_updated_at 
    BEFORE UPDATE ON transaction 
    FOR EACH ROW 
    EXECUTE FUNCTION update_updated_at_column();

GRANT SELECT, INSERT, UPDATE, DELETE ON transaction TO juspay_user;
GRANT SELECT, INSERT, UPDATE, DELETE ON audit_log TO juspay_user;
GRANT USAGE, SELECT ON SEQUENCE audit_log_id_seq TO juspay_user;
