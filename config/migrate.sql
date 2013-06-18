CREATE  TABLE IF NOT EXISTS `migrate` (
  `id` INT(10) NOT NULL AUTO_INCREMENT,
  `version` VARCHAR(50) NOT NULL,
  PRIMARY KEY (`id`) ,
  UNIQUE INDEX `id_UNIQUE` (`id` ASC)
  )
ENGINE = InnoDB
DEFAULT CHARACTER SET = utf8
COLLATE = utf8_general_ci;

ALTER TABLE `migrate`
ADD UNIQUE INDEX `version_UNIQUE` (`version` ASC) ;
