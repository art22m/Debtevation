# Base image
FROM haskell:8.10.4

# Set the working directory
WORKDIR /app

# Copy the project files to the container
COPY . /app

# Install project dependencies
RUN cabal update && cabal install --only-dependencies -j4

# Build the project
RUN cabal build

# Expose the port on which the application is running
EXPOSE 8080

# Start the application
CMD ["cabal", "run"]
