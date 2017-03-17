<?php

require_once('phpmailer/class.phpmailer.php');

$mail = new PHPMailer();

if( $_SERVER['REQUEST_METHOD'] == 'POST' ) {
    if( $_POST['wedding-rsvp-name'] != '' AND $_POST['wedding-rsvp-email'] != '' ) {

        $name = $_POST['wedding-rsvp-name'];
        $email = $_POST['wedding-rsvp-email'];
        $guests = $_POST['wedding-rsvp-guests'];
        $events = $_POST['wedding-rsvp-events'];

        $subject = 'Wedding RSVP Confirmation';

        $botcheck = $_POST['wedding-rsvp-botcheck'];

        $toemail = 'username@email.com'; // Your Email Address
        $toname = 'Your Name'; // Your Name

        if( $botcheck == '' ) {

            $mail->SetFrom( $email , $name );
            $mail->AddReplyTo( $email , $name );
            $mail->AddAddress( $toemail , $toname );
            $mail->Subject = $subject;

            $name = isset($name) ? "Name: $name<br><br>" : '';
            $email = isset($email) ? "Email: $email<br><br>" : '';
            $guests = isset($guests) ? "Guests: $guests<br><br>" : '';
            $events = isset($events) ? "Event: $events<br><br>" : '';

            $referrer = $_SERVER['HTTP_REFERER'] ? '<br><br><br>This Form was submitted from: ' . $_SERVER['HTTP_REFERER'] : '';

            $body = "$name $email $guests $events $referrer";

            $mail->MsgHTML( $body );
            $sendEmail = $mail->Send();

            if( $sendEmail == true ):
                echo 'Thank you for Confirming your RSVP.';
            else:
                echo 'Sorry couldn\'t confirm your RSVP. Please Try Again later.<br /><br /><strong>Reason:</strong><br />' . $mail->ErrorInfo . '';
            endif;
        } else {
            echo 'Bot <strong>Detected</strong>.! Clean yourself Botster.!';
        }
    } else {
        echo 'Please <strong>Fill up</strong> all the Fields and Try Again.';
    }
} else {
    echo 'An <strong>unexpected error</strong> occured. Please Try Again later.';
}

?>