<?php

require_once('phpmailer/class.phpmailer.php');

$mail = new PHPMailer();

if( $_SERVER['REQUEST_METHOD'] == 'POST' ) {

	$fname = $_POST['template-jobform-fname'];
	$lname = $_POST['template-jobform-lname'];
	$email = $_POST['template-jobform-email'];
	$age = $_POST['template-jobform-age'];
	$city = $_POST['template-jobform-city'];
	$position = $_POST['template-jobform-position'];
	$salary = $_POST['template-jobform-salary'];
	$start = $_POST['template-jobform-start'];
	$website = $_POST['template-jobform-website'];
	$experience = $_POST['template-jobform-experience'];
	$application = $_POST['template-jobform-application'];

	$name = $fname . ' ' . $lname;

	$subject = 'New Job Application';

	$botcheck = $_POST['template-jobform-botcheck'];

	$toemail = 'username@email.com'; // Your Email Address
	$toname = 'Your Name'; // Your Name

	if( $botcheck == '' ) {

		$mail->SetFrom( $email , $name );
		$mail->AddReplyTo( $email , $name );
		$mail->AddAddress( $toemail , $toname );
		$mail->Subject = $subject;

		$name = isset($name) ? "Name: $name<br><br>" : '';
		$email = isset($email) ? "Email: $email<br><br>" : '';
		$age = isset($age) ? "Age: $age<br><br>" : '';
		$city = isset($city) ? "City: $city<br><br>" : '';
		$position = isset($position) ? "Position: $position<br><br>" : '';
		$salary = isset($salary) ? "Salary: $salary<br><br>" : '';
		$start = isset($start) ? "Start: $start<br><br>" : '';
		$website = isset($website) ? "Website: $website<br><br>" : '';
		$experience = isset($experience) ? "Experience: $experience<br><br>" : '';
		$application = isset($application) ? "Application: $application<br><br>" : '';

		$referrer = $_SERVER['HTTP_REFERER'] ? '<br><br><br>This Form was submitted from: ' . $_SERVER['HTTP_REFERER'] : '';

		$body = "$name $email $age $city $position $salary $start $website $experience $application $referrer";

		$mail->MsgHTML( $body );
		$sendEmail = $mail->Send();

		if( $sendEmail == true ):
			echo 'We have <strong>successfully</strong> received your Application and will get Back to you as soon as possible.';
		else:
			echo 'Email <strong>could not</strong> be sent due to some Unexpected Error. Please Try Again later.<br /><br /><strong>Reason:</strong><br />' . $mail->ErrorInfo . '';
		endif;
	} else {
		echo 'Bot <strong>Detected</strong>.! Clean yourself Botster.!';
	}
} else {
	echo 'An <strong>unexpected error</strong> occured. Please Try Again later.';
}

?>