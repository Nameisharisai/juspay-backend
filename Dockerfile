# Multi-stage Docker build for production-ready Juspay Backend
FROM haskell:9.2-slim as builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    libpq-dev \
    libssl-dev \
    pkg-config \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy cabal files first for better caching
COPY *.cabal ./
COPY cabal.project* ./

# Update package index
RUN cabal update

# Build dependencies (this layer will be cached if dependencies don't change)
RUN cabal build --dependencies-only --enable-optimization

# Copy source code
COPY . .

# Build the application
RUN cabal build --enable-optimization

# Install the executable
RUN cabal install --install-method=copy --installdir=/app/bin

# Production stage
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libpq5 \
    libssl3 \
    ca-certificates \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create app user for security
RUN useradd --create-home --shell /bin/bash app

# Set working directory
WORKDIR /home/app

# Copy the built executable
COPY --from=builder /app/bin/juspay-backend ./

# Copy configuration files if needed
COPY --from=builder /app/schema.sql ./

# Change ownership to app user
RUN chown -R app:app /home/app

# Switch to app user
USER app

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:8080/api/v1/health || exit 1

# Expose port
EXPOSE 8080

# Set environment variables with defaults
ENV PORT=8080
ENV LOG_LEVEL=INFO
ENV MAX_CONNECTIONS=10
ENV REQUEST_TIMEOUT=30

# Run the application
CMD ["./juspay-backend"]