{
    // ===== Contract Information ===== //
    // Name: Bene Fundraising Platform  Dev Fee Contract
    // Description: Contract guarding the fee box for the Bene Fundraising Platform.
    // Version: 1.0.0
    // Based on: Phoenix HodlERG Fee (originally)

    // ===== Box Contents ===== //
    // Tokens
    // (Optional) (id, amount) - Token to be distributed (if not ERG)
    // Registers
    // R4: Coll[Byte] (Optional) - Token ID to distribute. Empty/absent for ERG, token ID for token distributions

    // ===== Relevant Transactions ===== //
    // 1. Fee Distribution Tx
    // Inputs: BeneFee1, ... , BeneFeeM
    // DataInputs: None
    // Outputs: Bruno, Lgd, Josemi, The Stable Order
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // bruno: reciver address
    // lgd:   reciver address
    // jm:    reciver address
    // order: rediver address

    // ===== Context Variables (_) ===== //
    // None

    // ===== Relevant Variables ===== //
    val feeDenom: Long   = 100L
    val brunoNum: Long   = 32L  // Bruno
    val lgdNum: Long = 32L     // Lgd
    val jmNum: Long = 32L     // Jossemi
    val orderNum: Long = 4L  // The Stable Order

    val brunoAddress: SigmaProp   = PK("`+bruno+`")
    val lgdAddress: SigmaProp = PK("`+lgd+`")
    val jmAddress: SigmaProp = PK("`+jm+`")
    val orderAddress: SigmaProp = PK("`+order+`")

    // Determine if distributing tokens or ERG from the actual box tokens, and validate R4 consistency
    val isTokenDistribution: Boolean = SELF.tokens.size > 0
    val distributionTokenId: Coll[Byte] = if (isTokenDistribution) {
        SELF.tokens(0)._1
    } else {
        Coll[Byte]()
    }
    val validR4: Boolean = if (SELF.R4[Coll[Byte]].isDefined && SELF.R4[Coll[Byte]].get.size > 0) {
        isTokenDistribution && SELF.R4[Coll[Byte]].get == distributionTokenId && SELF.tokens.size == 1
    } else {
        SELF.tokens.size <= 1
    }

    // Helper function to get distribution amount from box
    def getDistributionAmount(box: Box): Long = {
        if (isTokenDistribution) {
            // Get token amount
            val matchingTokens = box.tokens.filter { (token: (Coll[Byte], Long)) => 
                token._1 == distributionTokenId
            }
            if (matchingTokens.size > 0) {
                matchingTokens(0)._2
            } else {
                0L
            }
        } else {
            // Get ERG amount
            box.value
        }
    }

    // ===== Fee Distribution Tx (no miner fee) ===== //
    val validFeeDistributionTx: Boolean = {                         

        // Outputs: 4 outputs expected (bruno, lgd, jm, order)
        val brunoBoxOUT: Box    = OUTPUTS(0)
        val lgdBoxOUT: Box  = OUTPUTS(1)
        val jmBoxOUT: Box  = OUTPUTS(2)
        val orderBoxOUT: Box = OUTPUTS(3)

        // Prevent spending multiple dev fee boxes in a single transaction (double-satisfaction)
        val singleFeeInput: Boolean = INPUTS.filter({ (box: Box) => box.propositionBytes == SELF.propositionBytes }).size == 1

        // Calculate total output amount and required distribution amount from SELF
        val outputAmount: Long = OUTPUTS.map({ (output: Box) => output.value }).fold(0L, { (acc: Long, curr: Long) => acc + curr })
        val devAmount: Long = getDistributionAmount(SELF)

        val validMinAmount: Boolean = if (isTokenDistribution) {
            // For tokens, only check that we have minimum ERG for boxes
            outputAmount >= 5000000L
        } else {
            // For ERG, check total amount
            outputAmount >= 5000000L
        }
        
        val validDevBoxes: Boolean = {

            val brunoAmount: Long   = (brunoNum * devAmount) / feeDenom
            val lgdAmount: Long = (lgdNum * devAmount) / feeDenom
            val jmAmount: Long = (jmNum * devAmount) / feeDenom
            val orderAmount: Long = (orderNum * devAmount) / feeDenom

            val validBruno: Boolean   = (getDistributionAmount(brunoBoxOUT) == brunoAmount) && (brunoBoxOUT.propositionBytes == brunoAddress.propBytes)
            val validLgd: Boolean = (getDistributionAmount(lgdBoxOUT) == lgdAmount) && (lgdBoxOUT.propositionBytes == lgdAddress.propBytes)
            val validJm: Boolean = (getDistributionAmount(jmBoxOUT) == jmAmount) && (jmBoxOUT.propositionBytes == jmAddress.propBytes)
            val validOrder: Boolean = (getDistributionAmount(orderBoxOUT) == orderAmount) && (orderBoxOUT.propositionBytes == orderAddress.propBytes)

            allOf(Coll(
                validBruno,
                validLgd,
                validJm,
                validOrder
            ))

        }

        allOf(Coll(
            singleFeeInput,
            validR4,
            validMinAmount,
            validDevBoxes
        ))

    }

    sigmaProp(validFeeDistributionTx)

}
