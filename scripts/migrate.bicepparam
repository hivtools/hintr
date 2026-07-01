using 'migrate.bicep'

param hintResourceGroup = 'nmHint-RG'

param redisName = '${prefix}-hintr-queue'
param redisDbName = 'default'
param redisPrivateDnsZoneName = 'privatelink.eastus2.redis.azure.net'