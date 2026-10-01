using 'migrate.bicep'

param redisName = 'nm-hintr-queue'
param redisDbName = 'default'
param redisPrivateDnsZoneName = 'privatelink.eastus2.redis.azure.net'
param vnetName = 'nm-hint-nw'
param naomiStorageAccountName = 'naomiappstorage'
param naomiStorageShareName = 'results-share'
